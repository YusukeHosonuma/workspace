//
//  FizzBuzzTests.swift
//  FizzBuzzTests
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import XCTest

/// test for FizzBuzz.swift
class FizzBuzzTests: XCTestCase {
    
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    /// test for ==, !=
    func testFizzBuzzType_equal() {
        
        // Fizz
        XCTAssert(FizzBuzzType.fizz == .fizz)
        XCTAssert(FizzBuzzType.fizz != .buzz)
        XCTAssert(FizzBuzzType.fizz != .fizzBuzz)
        XCTAssert(FizzBuzzType.fizz != .other(number: 1))
        
        // Buzz
        XCTAssert(FizzBuzzType.buzz == .buzz)
        XCTAssert(FizzBuzzType.buzz != .fizz)
        XCTAssert(FizzBuzzType.buzz != .fizzBuzz)
        XCTAssert(FizzBuzzType.buzz != .other(number: 1))
        
        // FizzBuzz
        XCTAssert(FizzBuzzType.fizzBuzz == .fizzBuzz)
        XCTAssert(FizzBuzzType.fizzBuzz != .fizz)
        XCTAssert(FizzBuzzType.fizzBuzz != .buzz)
        XCTAssert(FizzBuzzType.fizzBuzz != .other(number: 1))
        
        // Other(number: Int)
        XCTAssert(FizzBuzzType.other(number: 1) == .other(number: 1))
        XCTAssert(FizzBuzzType.other(number: 1) != .other(number: 2))
        XCTAssert(FizzBuzzType.other(number: 1) != .fizz)
        XCTAssert(FizzBuzzType.other(number: 1) != .buzz)
        XCTAssert(FizzBuzzType.other(number: 1) != .fizzBuzz)
    }
    
    /// test for FizzBuzzType.fromNumber
    func testFizzBuzzType_fromNumber() {
        
        // multiple 3 -> Fizz
        XCTAssert(FizzBuzzType.fromNumber(3) == .fizz)
        XCTAssert(FizzBuzzType.fromNumber(6) == .fizz)

        // multiple 5 -> Buzz
        XCTAssert(FizzBuzzType.fromNumber(5)  == .buzz)
        XCTAssert(FizzBuzzType.fromNumber(10) == .buzz)

        // multiple 15 -> FizzBuzz
        XCTAssert(FizzBuzzType.fromNumber(15) == .fizzBuzz)
        XCTAssert(FizzBuzzType.fromNumber(30) == .fizzBuzz)
        
        // others
        XCTAssert(FizzBuzzType.fromNumber(1)  == .other(number: 1))
        XCTAssert(FizzBuzzType.fromNumber(2)  == .other(number: 2))
        XCTAssert(FizzBuzzType.fromNumber(4)  == .other(number: 4))
        XCTAssert(FizzBuzzType.fromNumber(7)  == .other(number: 7))
        XCTAssert(FizzBuzzType.fromNumber(14) == .other(number: 14))
        XCTAssert(FizzBuzzType.fromNumber(16) == .other(number: 16))

    }
    
    /// test for fizzBuzz()
    func test_fizzBuzz() {

        // Fizz
        XCTAssertEqual(fizzBuzz(3), "Fizz")
        XCTAssertEqual(fizzBuzz(6), "Fizz")
        
        // Buzz
        XCTAssertEqual(fizzBuzz(5), "Buzz")
        XCTAssertEqual(fizzBuzz(10), "Buzz")
        
        // FizzBuzz
        XCTAssertEqual(fizzBuzz(15), "FizzBuzz")
        XCTAssertEqual(fizzBuzz(30), "FizzBuzz")
        
        // others
        XCTAssertEqual( fizzBuzz(1),  "1")
        XCTAssertEqual( fizzBuzz(2),  "2")
        XCTAssertEqual( fizzBuzz(4),  "4")
        XCTAssertEqual( fizzBuzz(7),  "7")
        XCTAssertEqual(fizzBuzz(14), "14")
        XCTAssertEqual(fizzBuzz(16), "16")
    }
}
