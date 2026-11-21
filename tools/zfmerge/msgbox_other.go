//go:build !windows

package main

import "fmt"

// showMessage: Windows 외(시험용)에서는 표준 출력으로 대신함
func showMessage(title, text string, isError bool) {
	fmt.Printf("[%s] %s\n", title, text)
}
