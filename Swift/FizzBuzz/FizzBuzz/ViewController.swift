//
//  ViewController.swift
//  FizzBuzz
//
//  Created by Yusuke on 6/21/15.
//  Copyright © 2015 Yusuke. All rights reserved.
//

import UIKit

class ViewController: UIViewController, UITextFieldDelegate {

    @IBOutlet weak var messageLabel: UILabel!
    
    // 世界のナベアツ
    let nabeatsu = Nabe { (number: Int) -> String? in
        return (number % 5 == 0) ? "わん！" : .none
    }

    var count: Int = 0
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    // Mark: - IBAction

    @IBAction func tapCountUp(_ sender: AnyObject) {
        count += 1
        messageLabel.text = self.nabeatsu.count(count)
    }
}

