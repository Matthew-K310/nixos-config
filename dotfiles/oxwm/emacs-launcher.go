// ~/.config/oxwm/emacs-launcher.go
// Emacs launcher for oxwm (X11 window manager)
// Uses wmctrl to focus the specific Emacs frame that handles the command
package main

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

// switchToWindowDesktop finds which desktop the window is on via wmctrl -l and switches to it.
// This is needed because oxwm doesn't set _NET_WM_DESKTOP on all windows,
// so wmctrl -a alone can't determine which desktop to switch to.
func switchToWindowDesktop(windowID string) {
	// Convert decimal window ID to hex for matching wmctrl -l output
	id, err := strconv.ParseInt(windowID, 10, 64)
	if err != nil {
		return
	}
	hexID := fmt.Sprintf("0x%08x", id)

	cmd := exec.Command("wmctrl", "-l")
	output, err := cmd.Output()
	if err != nil {
		return
	}

	for _, line := range strings.Split(string(output), "\n") {
		fields := strings.Fields(line)
		if len(fields) >= 2 && strings.EqualFold(fields[0], hexID) {
			exec.Command("wmctrl", "-s", fields[1]).Run()
			return
		}
	}
}

// focusEmacsWindowByID raises and focuses a specific Emacs window by X11 window ID
func focusEmacsWindowByID(windowID string) error {
	cmd := exec.Command("wmctrl", "-i", "-a", windowID)
	return cmd.Run()
}

// focusEmacsWindow raises and focuses any Emacs window (fallback)
func focusEmacsWindow() error {
	cmd := exec.Command("wmctrl", "-x", "-a", "emacs")
	return cmd.Run()
}

// executeEmacsCommandAndGetWindowID runs the command and returns the X11 window ID
// of the frame that handled it
func executeEmacsCommandAndGetWindowID(command string) (string, error) {
	// Wrap the command to execute it and return the window ID of the selected frame
	wrappedCmd := fmt.Sprintf("(progn %s (frame-parameter (selected-frame) 'outer-window-id))", command)
	cmd := exec.Command("emacsclient", "-e", wrappedCmd)
	output, err := cmd.Output()
	if err != nil {
		return "", err
	}
	// Output is quoted, e.g. "12345678" - strip quotes and whitespace
	windowID := strings.TrimSpace(string(output))
	windowID = strings.Trim(windowID, "\"")
	return windowID, nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: emacs-launcher <elisp-command>")
		fmt.Fprintln(os.Stderr, "Example: emacs-launcher '(universal-launcher-popup)'")
		os.Exit(1)
	}

	// Join all arguments in case command has spaces
	emacsCommand := strings.Join(os.Args[1:], " ")

	// Execute the command and get the window ID of the frame that handled it
	windowID, err := executeEmacsCommandAndGetWindowID(emacsCommand)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Warning: emacsclient failed: %v\n", err)
		os.Exit(1)
	}

	// Switch to the Emacs window's desktop and focus it
	if windowID != "" && windowID != "nil" {
		switchToWindowDesktop(windowID)
		if err := focusEmacsWindowByID(windowID); err != nil {
			focusEmacsWindow()
		}
	} else {
		focusEmacsWindow()
	}
}
