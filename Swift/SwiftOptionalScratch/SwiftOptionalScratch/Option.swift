//
//  Option.swift
//  SwiftOptionalScratch
//
//  Created by Yusuke on 9/16/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import Foundation

enum OptionError: Error {
    case forceUnwrapFailed
}

// like Optional type
enum Option<T> : ExpressibleByNilLiteral {
    
    case some(T)
    case none
    
    init(nilLiteral: ()) {
        self = .none
    }
    
    // map
    func map<U>(_ f: (T) -> U) -> Option<U> {
        switch self {
        case .some(let value):
            return .some(f(value))
        case .none:
            return .none
        }
    }
    
    // flatMap and like optional chaining
    func flatMap<U>(_ f: (T) -> Option<U>) -> Option<U> {
        switch self {
        case .some(let value):
            return f(value)
        case .none:
            return .none
        }
    }
    
    // like ! operator
    func get() throws -> T {
        switch self {
        case .some(let value):
            return value
        case .none:
            throw OptionError.forceUnwrapFailed
        }
    }
    
    // like ?? operator
    func getOrElse(_ t: T) -> T {
        switch self {
        case .some(let value):
            return value
        case .none:
            return t
        }
    }
}
