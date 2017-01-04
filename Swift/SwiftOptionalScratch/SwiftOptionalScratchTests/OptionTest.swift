//
//  OptionTest.swift
//  SwiftOptionalScratch
//
//  Created by Yusuke on 9/16/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import XCTest
@testable import SwiftOptionalScratch

extension String {
    func toInt() -> Option<Int> {
        if let value = Int(self) {
            return .some(value)
        } else {
            return .none
        }
    }
}

class Goast: ExpressibleByNilLiteral, CustomStringConvertible {
    let name: Option<String>
    init(name: String) {
        self.name = .some(name)
    }
    required init(nilLiteral: ()) {
        self.name = .none
    }
    var description: String {
        let string = name.getOrElse("No name.")
        return "\"\(string)\""
    }
}

class OptionTest: XCTestCase {

    override func setUp() {
        super.setUp()
    }
    
    override func tearDown() {
        super.tearDown()
    }

    func testExample() {
        
        let s1: Option<String> = .some("123")
        
        // switch-case statement
        switch s1 {
        case .some(let value):
            print("s1 is \(value).")
        case .none:
            print("s1 is None.")
        }
        
        // map
        let r1: Option<Int> = s1.map{ $0.characters.count }
        print("r1 is \(r1)")
        
        // flatMap
        let r2: Option<Int> = s1.flatMap{ $0.toInt() }
        print("r2 is \(r2)")
        
        // !
        let r3: String = try! s1.get()
        print("r3 is \(r3)")
        
        let s2: Option<String> = nil
        
        // EXE_BAD_INSTRUCTION!! because s2 is nil
        //let r4: String = try! s2.get()
        //print("r4 is \(r4)")
        
        // ??
        let r4: String = s1.getOrElse("default")
        let r5: String = s2.getOrElse("default")
        print("r4 is \(r4)")
        print("r4 is \(r5)")

        // NilLiteralConvertible
        let g1: Goast = Goast(name: "hina-chan")
        print("g1 is \(g1)")
        
        let g2: Goast = nil
        print("g2 is \(g2)")
    }
}
