//
//  ViewController.swift
//  MVPPattern
//
//  Created by Yusuke on 8/19/15.
//  Copyright (c) 2015 Yusuke. All rights reserved.
//

import UIKit

// Protocol for Presenter.
protocol IViewController {
    func getText1() -> String
    func getText2() -> String
    func setResultMessage(_ message: String)
}

class ViewController: UIViewController, IViewController {

    @IBOutlet weak var text1: UITextField!
    @IBOutlet weak var text2: UITextField!
    @IBOutlet weak var resultLabel: UILabel!
    
    var presenter: Presenter! // Use DI for better?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.presenter = Presenter(view: self)
    }
    
    // MARK: - Action

    @IBAction func touchAddButton(_ sender: AnyObject) {
        self.presenter.add()
    }
    
    // MARK: - IViewController
    
    func getText1() -> String {
        return self.text1.text!
    }
    
    func getText2() -> String {
        return self.text2.text!
    }
    
    func setResultMessage(_ message: String) {
        self.resultLabel.text = message
    }
}

