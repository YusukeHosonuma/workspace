package helper

import (
	"strings"
)

func ToArrayLiteral(xs []string) string {
	return "[" + strings.Join(xs, ", ") + "]"
}
