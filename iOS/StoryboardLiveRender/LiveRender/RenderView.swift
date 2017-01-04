//
//  RenderView.swift
//  LiveRender
//
//  Created by Yusuke on 3/9/15.
//  Copyright (c) 2015 Yusuke. All rights reserved.
//

import UIKit

@IBDesignable
class RenderView: UIView {
    
    @IBInspectable var color: UIColor = UIColor.red
    @IBInspectable var offset: CGFloat = 0.0
    @IBInspectable var gridSize: CGFloat = 10.0
    
    override func draw(_ rect: CGRect) {
        
        let context = UIGraphicsGetCurrentContext();

        context?.setLineWidth(0.2)
        
        context?.setStrokeColor(color.cgColor)
        
        // draw |
        for i in 0..<100 {
            let x: CGFloat = offset + CGFloat(i - 1) * gridSize
            context?.move(to: CGPoint(x: x, y: 0))
            context?.addLine(to: CGPoint(x: x, y: self.bounds.height))
        }
        
        // draw -
        for i in 0..<300 {
            let y: CGFloat = offset + CGFloat(i - 1) * gridSize
            context?.move(to: CGPoint(x: 0, y: y))
            context?.addLine(to: CGPoint(x: self.bounds.width, y: y))
        }
        
        context?.closePath()
        context?.strokePath()
    }
}
