//
//  Nabe.swift
//  FizzBuzz
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import Foundation

/// Nabeatsu of the world.

/// Enum
enum NabeType {
    
    case aho, other(number: Int), special(message: String)
    
    static func fromNumber(_ n: Int) -> NabeType {
        if (isBasicAho(n)) {
            return aho
        } else {
            return other(number: n)
        }
    }
    
    static func fromNumber(_ n: Int, f: (Int) -> String?) -> NabeType {
        switch f(n) {
        case .none:
            return isBasicAho(n) ? .aho : .other(number: n)
        case .some(let s):
            return .special(message: s)
        }
    }
    
    static func isBasicAho(_ n: Int) -> Bool {
        let factor  = n % 3 == 0
        let include = String(n).range(of: "3") != nil
        return factor || include
    }
}

/// ==
func ==(a: NabeType, b: NabeType) -> Bool {
    switch (a, b) {
    case (.aho, .aho):
        return true
    case (.other(let a0), .other(let b0)):
        return a0 == b0
    case (.special(let a0), .special(let b0)):
        return a0 == b0
    default:
        return false
    }
}

/// !=
func !=(a: NabeType, b: NabeType) -> Bool {
    return !(a == b)
}

/// Nabeatsu
class Nabe {
    
    let special: ((Int) -> String?)?
    
    init() {
        self.special = .none
    }
    
    init(special: @escaping (Int) -> String?) {
        self.special = special
    }
    
    func count(_ number: Int) -> String {
        
        let type: NabeType;
        
        switch self.special {
        case .none:
            type = NabeType.fromNumber(number)
        case .some(let f):
            type = NabeType.fromNumber(number, f: f)
        }
        
        return message(type)
    }
    
    fileprivate func message(_ type: NabeType) -> String {
        
        switch type {
        case .aho:
            return "さぁーん！"
        case .other(let number):
            return "\(number)"
        case .special(let message):
            return message
        }
    }
}
