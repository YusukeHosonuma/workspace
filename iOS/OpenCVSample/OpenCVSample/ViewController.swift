//
//  ViewController.swift
//  OpenCVSample
//
//  Created by Yusuke on 2/17/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    @IBAction func tapButton(sender: AnyObject) {

        let destination = SampleViewController()
        presentViewController(destination, animated: true, completion: nil)
    }
}

