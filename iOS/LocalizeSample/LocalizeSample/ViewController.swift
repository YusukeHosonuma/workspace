//
//  ViewController.swift
//  LocalizeSample
//
//  Created by Yusuke on 2017/04/15.
//
//

import UIKit

class ViewController: UIViewController {

    @IBOutlet weak var label1: UILabel!
    @IBOutlet weak var label2: UILabel!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        let greeting1 = NSLocalizedString("greeting", comment: "")
        let greeting2 = String(format: NSLocalizedString("greeting2", comment: ""), "Swift", 3.1)
        
        label1.text = greeting1
        label2.text = greeting2
    }
}

