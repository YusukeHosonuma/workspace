package chap6

import (
	"testing"
)

func TestSuite(t *testing.T) {
	var x IntSet

	// String()
	if x.String() != "{}" {
		t.Error()
	}

	// Add()
	x.Add(1)
	x.Add(144)
	x.Add(9)
	x.Add(9) // same value

	// String()
	if r := x.String(); r != "{1 9 144}" {
		t.Errorf("x.String() = %v", r)
	}

	// Len()
	if n := x.Len(); n != 3 {
		t.Errorf("x.Len() = %v", n)
	}

	// Has()
	tests := []struct {
		input  int
		expect bool
	}{
		{0, false},
		{1, true},
	}
	for _, test := range tests {
		if r := x.Has(test.input); r != test.expect {
			t.Errorf("x.Has(%v) = %v", test.input, r)
		}
	}
}

func TestRemove(t *testing.T) {
	var x IntSet
	x.Add(1)
	x.Add(9)
	x.Add(12)

	// Remove()
	x.Remove(9)
	x.Remove(144)

	// String()
	if r := x.String(); r != "{1 12}" {
		t.Errorf("x.String() = %v", r)
	}
}

func TestUnionWith(t *testing.T) {
	var x IntSet
	x.Add(1)
	x.Add(9)

	var y IntSet
	y.Add(1)
	y.Add(3)
	y.Add(144)

	x.UnionWith(&y)

	// String()
	if r := x.String(); r != "{1 3 9 144}" {
		t.Errorf("x.String() = %v", r)
	}
}
