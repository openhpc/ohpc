package main

import (
	"fmt"
	"runtime"
)

func main() {
	fmt.Printf("Hello from Go %s on %s/%s\n", runtime.Version(), runtime.GOOS,
		runtime.GOARCH)
}
