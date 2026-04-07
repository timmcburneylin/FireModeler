#!/usr/bin/env python
"""
Experimental Windows-only automation for FuelCalc's:
  Report -> Plot CSV File -> Data Only

Why this exists:
- FuelCalc batch runs generate summary CSVs, but not the per-plot "Data Only"
  exports that Holden uses downstream.
- FuelCalc does not appear to expose a documented CLI flag for this export.
- This script automates the GUI export against an already-open FuelCalc window.

Usage:
1. Open FuelCalc manually for the treatment you want to export.
2. Click the first plot row in the plot table so arrow keys advance plots.
3. Run:
     python scripts/fuelcalc_plot_csv_export.py export ^
       --project PROJECT ^
       --treatment TU_A ^
       --plot-count 10

Optional inspection mode:
     python scripts/fuelcalc_plot_csv_export.py inspect

Notes:
- This is intentionally conservative and explicit. GUI automation is brittle.
- The menu path is invoked by menu text lookup when available, with a
  keyboard-shortcut fallback for apps that do not expose a Win32 menu handle.
- Plot-to-plot movement currently assumes the Down arrow advances the selected
  plot after each export. If FuelCalc behaves differently on another machine,
  this script will need calibration.
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import Iterable, Optional

import win32api
import win32com.client
import win32con
import win32gui


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_PROJECTS = ROOT / "projects"


class AutomationError(RuntimeError):
    pass


def log(message: str) -> None:
    print(message, flush=True)


def enum_windows() -> list[tuple[int, str]]:
    windows: list[tuple[int, str]] = []

    def callback(hwnd: int, _: int) -> None:
        if not win32gui.IsWindowVisible(hwnd):
            return
        title = win32gui.GetWindowText(hwnd).strip()
        if title:
            windows.append((hwnd, title))

    win32gui.EnumWindows(callback, 0)
    return windows


def find_fuelcalc_window(title_contains: str = "FuelCalc") -> int:
    matches = [(hwnd, title) for hwnd, title in enum_windows() if title_contains.lower() in title.lower()]
    if not matches:
        raise AutomationError(
            f"No visible FuelCalc window found containing '{title_contains}'. "
            "Open FuelCalc to the target treatment first."
        )
    matches.sort(key=lambda item: len(item[1]))
    return matches[0][0]


def force_foreground(hwnd: int) -> None:
    shell = win32com.client.Dispatch("WScript.Shell")
    shell.SendKeys("%")
    win32gui.ShowWindow(hwnd, win32con.SW_RESTORE)
    win32gui.SetForegroundWindow(hwnd)
    time.sleep(0.2)


def menu_items(menu: int) -> list[tuple[int, str, int]]:
    count = win32gui.GetMenuItemCount(menu)
    items: list[tuple[int, str, int]] = []
    for idx in range(count):
        text = win32gui.GetMenuString(menu, idx, win32con.MF_BYPOSITION).replace("&", "").strip()
        submenu = win32gui.GetSubMenu(menu, idx)
        item_id = win32gui.GetMenuItemID(menu, idx)
        items.append((idx, text, submenu if submenu else item_id))
    return items


def get_menu_command_id(hwnd: int, labels: Iterable[str]) -> int:
    labels = list(labels)
    menu = win32gui.GetMenu(hwnd)
    if not menu:
        raise AutomationError("FuelCalc main window has no menu handle.")

    current_menu = menu
    submenu = None
    for depth, label in enumerate(labels):
        wanted = label.lower()
        found = None
        for idx, text, handle_or_id in menu_items(current_menu):
            if text.lower() == wanted:
                found = (idx, text, handle_or_id)
                break
        if not found:
            available = ", ".join(text or f"<item {idx}>" for idx, text, _ in menu_items(current_menu))
            raise AutomationError(
                f"Could not find menu item '{label}' at depth {depth + 1}. Available items: {available}"
            )

        _, _, handle_or_id = found
        if depth == len(labels) - 1:
            if handle_or_id == -1:
                raise AutomationError(f"Menu item '{label}' is a submenu, not a command.")
            return handle_or_id

        submenu = handle_or_id
        if submenu == -1:
            raise AutomationError(f"Menu item '{label}' is not a submenu.")
        current_menu = submenu

    raise AutomationError("Failed to resolve menu command id.")


def child_windows(hwnd: int) -> list[int]:
    children: list[int] = []
    win32gui.EnumChildWindows(hwnd, lambda child, _: children.append(child), 0)
    return children


def iter_descendants(hwnd: int) -> Iterable[int]:
    queue = [hwnd]
    while queue:
        current = queue.pop(0)
        for child in child_windows(current):
            yield child
            queue.append(child)


def dump_window_tree(hwnd: int) -> None:
    log(f"Main window: {hwnd} | {win32gui.GetWindowText(hwnd)}")
    for child in iter_descendants(hwnd):
        cls = win32gui.GetClassName(child)
        text = win32gui.GetWindowText(child)
        try:
            rect = win32gui.GetWindowRect(child)
        except win32gui.error:
            rect = None
        log(f"  hwnd={child} class={cls} text={text!r} rect={rect}")


def wait_for_dialog(title_contains: str = "Save", timeout: float = 10.0) -> int:
    deadline = time.time() + timeout
    while time.time() < deadline:
        for hwnd, title in enum_windows():
            cls = win32gui.GetClassName(hwnd)
            if cls == "#32770" and title_contains.lower() in title.lower():
                return hwnd
        time.sleep(0.1)
    raise AutomationError(f"Timed out waiting for dialog containing '{title_contains}'.")


def find_first_descendant_by_class(hwnd: int, class_names: set[str]) -> Optional[int]:
    for child in iter_descendants(hwnd):
        if win32gui.GetClassName(child) in class_names:
            return child
    return None


def find_button(hwnd: int, texts: Iterable[str]) -> Optional[int]:
    wanted = {text.lower() for text in texts}
    for child in iter_descendants(hwnd):
        if win32gui.GetClassName(child) != "Button":
            continue
        text = win32gui.GetWindowText(child).replace("&", "").strip().lower()
        if text in wanted:
            return child
    return None


def set_dialog_filename(dialog_hwnd: int, output_path: Path) -> None:
    edit = find_first_descendant_by_class(dialog_hwnd, {"Edit"})
    if not edit:
        raise AutomationError("Could not find filename edit box in Save dialog.")
    win32gui.SendMessage(edit, win32con.WM_SETTEXT, 0, str(output_path))
    time.sleep(0.1)

    save_button = find_button(dialog_hwnd, {"save", "open"})
    if not save_button:
        raise AutomationError("Could not find Save button in dialog.")
    win32gui.SendMessage(save_button, win32con.BM_CLICK, 0, 0)


def wait_for_dialog_to_close(dialog_hwnd: int, timeout: float = 10.0) -> None:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if not win32gui.IsWindow(dialog_hwnd):
            return
        time.sleep(0.1)
    raise AutomationError("Save dialog did not close after clicking Save.")


def send_down_key() -> None:
    shell = win32com.client.Dispatch("WScript.Shell")
    shell.SendKeys("{DOWN}")
    time.sleep(0.15)


def send_keys(sequence: str, delay: float = 0.2) -> None:
    shell = win32com.client.Dispatch("WScript.Shell")
    shell.SendKeys(sequence)
    time.sleep(delay)


def send_key_steps(sequence: str, default_delay: float = 0.25) -> None:
    """
    Send a sequence in smaller steps so menu navigation has time to react.

    Syntax:
    - Tokens wrapped in [] are sent as a single SendKeys call, e.g. [F10], [%r]
    - A token may include a repeat count after `*`, e.g. [DOWN*3]
    - All other characters are sent one at a time
    """
    i = 0
    while i < len(sequence):
        if sequence[i] == "[":
            j = sequence.find("]", i + 1)
            if j == -1:
                raise AutomationError(f"Malformed key sequence: {sequence}")
            token = sequence[i + 1 : j]
            repeat = 1
            if "*" in token:
                token, repeat_text = token.split("*", 1)
                repeat = int(repeat_text)
            token_upper = token.upper()
            key_names = {
                "F10",
                "LEFT",
                "RIGHT",
                "UP",
                "DOWN",
                "ENTER",
                "TAB",
                "ESC",
            }
            for _ in range(repeat):
                if token_upper in key_names:
                    send_keys("{" + token_upper + "}", delay=default_delay)
                else:
                    send_keys(token, delay=default_delay)
            i = j + 1
            continue

        send_keys(sequence[i], delay=default_delay)
        i += 1


def click_point(x: int, y: int, delay: float = 0.25) -> None:
    win32api.SetCursorPos((x, y))
    time.sleep(0.1)
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
    time.sleep(delay)


def parse_mouse_points(sequence: str) -> list[tuple[int, int]]:
    points: list[tuple[int, int]] = []
    for part in sequence.split(";"):
        part = part.strip()
        if not part:
            continue
        x_text, y_text = part.split(",", 1)
        points.append((int(x_text.strip()), int(y_text.strip())))
    if not points:
        raise AutomationError("Mouse sequence is empty.")
    return points


def activate_plot_csv_export_with_mouse(hwnd: int, mouse_sequence: str) -> None:
    left, top, _, _ = win32gui.GetWindowRect(hwnd)
    for rel_x, rel_y in parse_mouse_points(mouse_sequence):
        click_point(left + rel_x, top + rel_y, delay=0.3)


def activate_plot_csv_export(hwnd: int, mode: str, key_sequence: str, mouse_sequence: str) -> None:
    if mode in {"auto", "menu"}:
        try:
            command_id = get_menu_command_id(hwnd, ["Report", "Plot CSV File", "Data Only"])
            win32gui.PostMessage(hwnd, win32con.WM_COMMAND, command_id, 0)
            return
        except AutomationError:
            if mode == "menu":
                raise

    force_foreground(hwnd)

    if mode == "mouse":
        activate_plot_csv_export_with_mouse(hwnd, mouse_sequence)
        return

    send_key_steps(key_sequence, default_delay=0.35)


def project_plot_dir(project: str, treatment: str) -> Path:
    return DEFAULT_PROJECTS / project / "data" / "raw" / "FuelCalc" / "Outputs" / treatment / "Plot Files"


def export_plot_csvs(
    project: str,
    treatment: str,
    plot_count: int,
    delay: float,
    save_timeout: float,
    mode: str,
    key_sequence: str,
    mouse_sequence: str,
) -> None:
    out_dir = project_plot_dir(project, treatment)
    out_dir.mkdir(parents=True, exist_ok=True)

    hwnd = find_fuelcalc_window()
    force_foreground(hwnd)

    log(f"Attached to FuelCalc window: {win32gui.GetWindowText(hwnd)}")
    log(f"Exporting {plot_count} plot CSVs into: {out_dir}")

    for idx in range(1, plot_count + 1):
        output_file = out_dir / f"Plot {idx}.csv"
        log(f"[{idx}/{plot_count}] Exporting {output_file.name}")

        activate_plot_csv_export(hwnd, mode=mode, key_sequence=key_sequence, mouse_sequence=mouse_sequence)
        dialog = wait_for_dialog("Save", timeout=save_timeout)
        set_dialog_filename(dialog, output_file)
        wait_for_dialog_to_close(dialog, timeout=save_timeout)

        time.sleep(delay)
        if idx < plot_count:
            send_down_key()

    log("FuelCalc plot export finished.")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Experimental FuelCalc plot CSV GUI automation")
    subparsers = parser.add_subparsers(dest="command", required=True)

    inspect_parser = subparsers.add_parser("inspect", help="Dump the current FuelCalc window tree")
    inspect_parser.add_argument("--title-contains", default="FuelCalc")

    export_parser = subparsers.add_parser("export", help="Export Plot CSV Data Only files")
    export_parser.add_argument("--project", required=True, help="Project folder name under projects/")
    export_parser.add_argument("--treatment", required=True, help="Treatment folder name, e.g. TU_A")
    export_parser.add_argument("--plot-count", required=True, type=int, help="Number of plots to export")
    export_parser.add_argument(
        "--delay",
        type=float,
        default=0.4,
        help="Delay in seconds after each save to let FuelCalc return to the plot table",
    )
    export_parser.add_argument(
        "--save-timeout",
        type=float,
        default=10.0,
        help="Timeout in seconds to wait for each Save dialog",
    )
    export_parser.add_argument(
        "--mode",
        choices=["auto", "menu", "keys", "mouse"],
        default="auto",
        help="How to trigger the export menu. 'auto' tries Win32 menu first, then key sequence.",
    )
    export_parser.add_argument(
        "--key-sequence",
        default="[F10][RIGHT][DOWN*3][RIGHT]",
        help="Stepwise menu sequence. Tokens in [] are special keys or grouped SendKeys calls.",
    )
    export_parser.add_argument(
        "--mouse-sequence",
        default="56,40;45,87;131,87",
        help="Relative click points from the top-left of the FuelCalc window: menu; submenu; final item.",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    try:
        args = parse_args(argv)
        if args.command == "inspect":
            hwnd = find_fuelcalc_window(args.title_contains)
            dump_window_tree(hwnd)
            return 0

        if args.command == "export":
            export_plot_csvs(
                project=args.project,
                treatment=args.treatment,
                plot_count=args.plot_count,
                delay=args.delay,
                save_timeout=args.save_timeout,
                mode=args.mode,
                key_sequence=args.key_sequence,
                mouse_sequence=args.mouse_sequence,
            )
            return 0

        raise AutomationError(f"Unsupported command: {args.command}")
    except AutomationError as exc:
        log(f"ERROR: {exc}")
        return 1
    except KeyboardInterrupt:
        log("Cancelled.")
        return 130


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
