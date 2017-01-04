//
//  TestCollectionViewController.swift
//  CollectionViewTest
//
//  Created by Yusuke on 11/13/14.
//  Copyright (c) 2014 yusuke.hosonuma. All rights reserved.
//

import UIKit

let reuseIdentifier = "Cell"

class TestCollectionViewController: UICollectionViewController {

    var modeIndex = 0
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.collectionView?.register(UICollectionViewCell.self, forCellWithReuseIdentifier: reuseIdentifier)
        
        self.change();
     
        let changeButton = UIBarButtonItem(title: "change", style: UIBarButtonItemStyle.plain, target: self, action:#selector(TestCollectionViewController.change))
        self.navigationItem.rightBarButtonItem = changeButton
    }
    
    func change() {

        self.modeIndex = (self.modeIndex + 1) % 2

        switch (modeIndex) {
        case 0:
            let layout = UICollectionViewFlowLayout();
            layout.itemSize = CGSize(width:150, height: 150)
            self.collectionView?.setCollectionViewLayout(layout, animated: true)

        case 1:
            let layout = UICollectionViewFlowLayout();
            layout.itemSize = CGSize(width:320, height: 80)
            self.collectionView?.setCollectionViewLayout(layout, animated: true)
            
        default:
            break
        }
        
        self.collectionView?.reloadData()
        
        self.collectionView?.visibleCells.map{
            (object: AnyObject) -> () in
            let cell = object as! UICollectionViewCell
            if let indexPath = self.collectionView?.indexPath(for: cell) {
                self.updateCell(cell, indexPath:indexPath)
            }
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        //#warning Incomplete method implementation -- Return the number of sections
        return 1
    }


    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        //#warning Incomplete method implementation -- Return the number of items in the section
        return 10
    }

    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: reuseIdentifier, for: indexPath) 
        self.updateCell(cell, indexPath: indexPath)
    
        return cell
    }
    
    func updateCell(_ cell: UICollectionViewCell, indexPath: IndexPath) {
        
        switch (modeIndex) {
        case 0:
            cell.backgroundColor = UIColor.blue
            break
            
        case 1:
            cell.backgroundColor = UIColor.red
            break
            
        default:
            break
        }
    }

    // MARK: UICollectionViewDelegate

    /*
    // Uncomment this method to specify if the specified item should be highlighted during tracking
    override func collectionView(collectionView: UICollectionView, shouldHighlightItemAtIndexPath indexPath: NSIndexPath) -> Bool {
        return true
    }
    */

    /*
    // Uncomment this method to specify if the specified item should be selected
    override func collectionView(collectionView: UICollectionView, shouldSelectItemAtIndexPath indexPath: NSIndexPath) -> Bool {
        return true
    }
    */

    /*
    // Uncomment these methods to specify if an action menu should be displayed for the specified item, and react to actions performed on the item
    override func collectionView(collectionView: UICollectionView, shouldShowMenuForItemAtIndexPath indexPath: NSIndexPath) -> Bool {
        return false
    }

    override func collectionView(collectionView: UICollectionView, canPerformAction action: Selector, forItemAtIndexPath indexPath: NSIndexPath, withSender sender: AnyObject?) -> Bool {
        return false
    }

    override func collectionView(collectionView: UICollectionView, performAction action: Selector, forItemAtIndexPath indexPath: NSIndexPath, withSender sender: AnyObject?) {
    
    }
    */

}
