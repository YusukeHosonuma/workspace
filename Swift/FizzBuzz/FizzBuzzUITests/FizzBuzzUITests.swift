//
//  FizzBuzzUITests.swift
//  FizzBuzzUITests
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import Foundation
import XCTest

class FizzBuzzUITests: XCTestCase {
        
    override func setUp() {
        super.setUp()
        
        // Put setup code here. This method is called before the invocation of each test method in the class.
        
        // In UI tests it is usually best to stop immediately when a failure occurs.
        continueAfterFailure = false
        // UI tests must launch the application that they test. Doing this in setup will make sure it happens for each test method.
        XCUIApplication().launch()
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func testExample() {

        let app = XCUIApplication()
        
        let button = app.buttons["数える！"]
        let label  = app.staticTexts.element(at: 0)
        
        button.tap()
        XCTAssertEqual(label.label, "1")
        
        button.tap()
        XCTAssertEqual(label.label, "2")

        // Aho
        button.tap()
        XCTAssertEqual(label.label, "さぁーん！")

        button.tap()
        XCTAssertEqual(label.label, "4")

        // Dog!
        button.tap()
        XCTAssertEqual(label.label, "わん！")

        // Aho
        button.tap()
        XCTAssertEqual(label.label, "さぁーん！")

        button.tap()
        XCTAssertEqual(label.label, "7")
    }
}
