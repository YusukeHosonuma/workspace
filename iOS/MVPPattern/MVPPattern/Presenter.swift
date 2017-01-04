//
//  Presenter.swift
//  MVPPattern
//
//  Created by Yusuke on 8/19/15.
//  Copyright (c) 2015 Yusuke. All rights reserved.
//

import UIKit

class Presenter: AnyObject {
   
    let view: IViewController // Not depends View!
    let calculator: Calculator
    
    init(view: IViewController) {
        self.view = view
        self.calculator = Calculator()
    }
    
    func add() {
        
        let text1 = self.view.getText1()
        let text2 = self.view.getText2()
        
        // Use 0 if NaN.
        let num1 = Int(text1) ?? 0
        let num2 = Int(text2) ?? 0
        
        // Work with the Model.
        let result = self.calculator.add(num1, num2)
        
        // Apply to view.
        let resultMessage = "Result: \(result)"
        self.view.setResultMessage(resultMessage)
    }
}
