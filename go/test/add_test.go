package test

import (
	"testing"
	"testing/quick"
)

func TestAdd(t *testing.T) {
	tests := []struct {
		x    int
		y    int
		want int
	}{
		{x: 0, y: 1, want: 1}, // 0 + 1 = 1
		{x: 1, y: 1, want: 2}, // 1 + 1 = 2
	}

	for _, tt := range tests {
		if got := Add(tt.x, tt.y); got != tt.want {
			t.Errorf("Add() = %v, want %v", got, tt.want)
		}
	}
}

func TestAddQuick(t *testing.T) {

	f := func(x, y int) bool {
		return Add(x, y) == Add(y, x)
	}

	if err := quick.Check(f, nil); err != nil {
		t.Error(err.Error())
	}
}
