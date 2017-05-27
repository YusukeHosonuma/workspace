
import Foundation

func isPalindrome(string: String) -> Bool {
    return string == String(string.characters.reverse())
}

var i = 10

repeat {
    
    let b = String(i, radix: 2)
    let o = String(i, radix: 8)
    let n = String(i)
    
    let result = isPalindrome(b) && isPalindrome(o) && isPalindrome(n)
    if result {
        print("Anser:")
        print("10進数: " + n)
        print(" 2進数: " + b)
        print(" 8進数: " + o)
        break
    }
    i += 1
} while true
