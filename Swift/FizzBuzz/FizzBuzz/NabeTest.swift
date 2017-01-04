//
//  NabeTest.swift
//  FizzBuzz
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import XCTest

class NabeTest: XCTestCase {

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    /// test for ==, !=
    func testNabeType_equal() {
        
        // Aho
        XCTAssert(NabeType.aho == NabeType.aho)
        XCTAssert(NabeType.aho != NabeType.other(number: 1))
        XCTAssert(NabeType.aho != NabeType.special(message: "message"))
        
        // Other(number: Int)
        XCTAssert(NabeType.other(number: 1) == .other(number: 1))
        XCTAssert(NabeType.other(number: 1) != .other(number: 2))
        XCTAssert(NabeType.other(number: 1) != .aho)
        XCTAssert(NabeType.other(number: 1) != .special(message: "message"))
        
        // Special
        XCTAssert(NabeType.special(message: "message1") == NabeType.special(message: "message1"))
        XCTAssert(NabeType.special(message: "message1") != NabeType.special(message: "message2"))
        XCTAssert(NabeType.special(message: "message1") != NabeType.aho)
        XCTAssert(NabeType.special(message: "message1") != NabeType.other(number: 1))
    }
    
    /// test for NabeType.fromNumber(Int)
    func testNabeType_fromNumber() {
        
        // multiple 3 -> Aho
        XCTAssert(NabeType.fromNumber(3) == .aho)
        XCTAssert(NabeType.fromNumber(6) == .aho)
        
        // include 3 -> Aho
        XCTAssert(NabeType.fromNumber(13) == .aho)
        XCTAssert(NabeType.fromNumber(31) == .aho)
        
        // others
        XCTAssert(NabeType.fromNumber(1)  == .other(number: 1))
        XCTAssert(NabeType.fromNumber(2)  == .other(number: 2))
        XCTAssert(NabeType.fromNumber(4)  == .other(number: 4))
        XCTAssert(NabeType.fromNumber(14) == .other(number: 14))
    }
    
    /// test for NabeType.fromNumber(Int, Int -> Bool)
    func testNabeType_fromNumber2() {
        
        let fiveDog : ((Int) -> String?) = {
            if ($0 % 5 == 0) {
                return "わん！"
            } else {
                return Optional.none
            }
        }

        // Special
        XCTAssert(NabeType.fromNumber( 5, f: fiveDog) == .special(message: "わん！"))
        XCTAssert(NabeType.fromNumber(10, f: fiveDog) == .special(message: "わん！"))
        XCTAssert(NabeType.fromNumber(15, f: fiveDog) == .special(message: "わん！")) // Note: Special > Aho
        
        // multiple 3 -> Aho
        XCTAssert(NabeType.fromNumber(3, f: fiveDog) == .aho)
        XCTAssert(NabeType.fromNumber(6, f: fiveDog) == .aho)

        // include 3 -> Aho
        XCTAssert(NabeType.fromNumber(13, f: fiveDog) == .aho)
        XCTAssert(NabeType.fromNumber(31, f: fiveDog) == .aho)

        // others
        XCTAssert(NabeType.fromNumber( 1, f: fiveDog) == .other(number: 1))
        XCTAssert(NabeType.fromNumber( 2, f: fiveDog) == .other(number: 2))
        XCTAssert(NabeType.fromNumber( 4, f: fiveDog) == .other(number: 4))
        XCTAssert(NabeType.fromNumber(14, f: fiveDog) == .other(number: 14))
    }

    /// test for Nabe(normal)
    func test_Nabe_normal() {
        
        let nabe = Nabe()
        
        // Aho by multiple 3
        XCTAssertEqual(nabe.count(3), "さぁーん！")
        XCTAssertEqual(nabe.count(6), "さぁーん！")
        
        // Aho by include 3
        XCTAssertEqual(nabe.count(13), "さぁーん！")
        XCTAssertEqual(nabe.count(31), "さぁーん！")
        
        // others
        XCTAssertEqual(nabe.count(1),  "1")
        XCTAssertEqual(nabe.count(2),  "2")
        XCTAssertEqual(nabe.count(4),  "4")
        XCTAssertEqual(nabe.count(14), "14")
    }
    
    /// test for Nabe(five Dog!)
    func test_Nabe_fiveDog() {
        
        // Dog by multiple 5
        let fiveDog = { (number) -> String? in
            return number % 5 == 0 ? "わん！" : .none
        }
        
        let nabe = Nabe(special: fiveDog)
        
        // Dog by multiple 5
        XCTAssertEqual(nabe.count(5),  "わん！")
        XCTAssertEqual(nabe.count(15), "わん！")
        
        // Aho by multiple 3
        XCTAssertEqual(nabe.count(3), "さぁーん！")
        XCTAssertEqual(nabe.count(6), "さぁーん！")
        
        // Aho by include 3
        XCTAssertEqual(nabe.count(13), "さぁーん！")
        XCTAssertEqual(nabe.count(31), "さぁーん！")
        
        // others
        XCTAssertEqual(nabe.count(1),  "1")
        XCTAssertEqual(nabe.count(2),  "2")
        XCTAssertEqual(nabe.count(4),  "4")
        XCTAssertEqual(nabe.count(14), "14")
    }
}
