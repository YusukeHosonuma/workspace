//
//  FizzBuzz.swift
//  FizzBuzz
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import Foundation

/// FizzBuzz problem.

enum FizzBuzzType {
    
    case fizz, buzz, fizzBuzz, other(number: Int)
    
    static func fromNumber(_ n: Int) -> FizzBuzzType {
        let fizz = n % 3 == 0
        let buzz = n % 5 == 0
        if (fizz && buzz) {
            return .fizzBuzz
        } else if (fizz) {
            return .fizz
        } else if (buzz) {
            return .buzz
        } else {
            return .other(number: n)
        }
    }
}

func ==(a: FizzBuzzType, b: FizzBuzzType) -> Bool {
    switch (a, b) {
    case (.fizz, .fizz):
        return true
    case (.buzz, .buzz):
        return true
    case (.fizzBuzz, .fizzBuzz):
        return true
    case (.other(let a0), .other(let b0)):
        return a0 == b0
    default:
        return false
    }
}

func !=(a: FizzBuzzType, b: FizzBuzzType) -> Bool {
    return !(a == b)
}

func fizzBuzz(_ n: Int) -> String {
    switch FizzBuzzType.fromNumber(n) {
    case .fizz:
        return "Fizz"
    case .buzz:
        return "Buzz"
    case .fizzBuzz:
        return "FizzBuzz"
    case .other(let number):
        return "\(number)"
    }
}
