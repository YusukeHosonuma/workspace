//
//  ViewController.swift
//  SaveRestore2
//
//  Created by Yusuke on 12/25/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

import UIKit

class ViewController: UIViewController { //, UIViewControllerRestoration {

    required init(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)!
//        self.restorationClass = ViewController.self
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    override func encodeRestorableState(with coder: NSCoder) {
        super.encodeRestorableState(with: coder)
        print("🚗encodeRestorableState")
    }
    
    override func decodeRestorableState(with coder: NSCoder) {
        super.decodeRestorableState(with: coder)
        print("🚙decodeRestorableState")
    }
    
    override func applicationFinishedRestoringState() {
        print("✈applicationFinishedRestoringState")
    }
    
//    static func viewController(withRestorationIdentifierPath identifierComponents: [Any], coder: NSCoder) -> UIViewController? {
//        return UIViewController()
//    }
}

