//go:build windows

package main

import (
	"syscall"
	"unsafe"
)

// showMessage: Windows 메시지 창 (MessageBoxW)
func showMessage(title, text string, isError bool) {
	user32 := syscall.NewLazyDLL("user32.dll")
	proc := user32.NewProc("MessageBoxW")
	t, _ := syscall.UTF16PtrFromString(text)
	c, _ := syscall.UTF16PtrFromString(title)
	var flags uintptr = 0x40 // MB_ICONINFORMATION
	if isError {
		flags = 0x10 // MB_ICONERROR
	}
	proc.Call(0, uintptr(unsafe.Pointer(t)), uintptr(unsafe.Pointer(c)), flags|0x40000) // MB_TOPMOST
}
