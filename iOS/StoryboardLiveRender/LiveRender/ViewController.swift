//
//  ViewController.swift
//  LiveRender
//
//  Created by Yusuke on 3/9/15.
//  Copyright (c) 2015 Yusuke. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        
        let view = RenderView()
        view.frame = CGRect(x: 0, y: 0, width: 300, height: 300)
        view.backgroundColor = UIColor.clear
        self.view.addSubview(view)
        // Do any additional setup after loading the view, typically from a nib.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }


}

