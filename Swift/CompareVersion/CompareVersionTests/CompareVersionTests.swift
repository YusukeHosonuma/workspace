//
//  CompareVersionTests.swift
//  CompareVersionTests
//
//  Created by Yusuke on 12/15/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

import XCTest

class VersionStringTests: XCTestCase {
    
    override func setUp() {
        super.setUp()
    }
    
    override func tearDown() {
        super.tearDown()
    }
    
    func test_compare() {
        XCTAssertTrue(Version(string:   "1.0.0") == Version(string: "1.0.0"))
        XCTAssertTrue(Version(string:   "1.0.0") >= Version(string: "1.0.0"))
        XCTAssertTrue(Version(string:   "1.0.0") <= Version(string: "1.0.0"))
        XCTAssertTrue(Version(string:   "1.2.0") >  Version(string: "1.1.0"))
        XCTAssertTrue(Version(string:  "1.10.0") >  Version(string: "1.2.0"))
        XCTAssertTrue(Version(string: "1.1.0.1") >  Version(string: "1.1.0"))
    }
}

struct Version {
    let string: String
    func toInt() -> [Int] {
        return string.components(separatedBy: ".").map{ Int($0) ?? 0 }
    }
}

extension Version : Comparable {}

func == (lhs: Version, rhs: Version) -> Bool {
    return lhs.string == rhs.string
}

func < (lhs: Version, rhs: Version) -> Bool {
    return lhs.toInt().lexicographicallyPrecedes(rhs.toInt())
}
