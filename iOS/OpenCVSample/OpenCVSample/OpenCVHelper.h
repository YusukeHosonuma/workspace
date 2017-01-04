//
//  OpenCVHelper.h
//  OpenCVSample
//
//  Created by Yusuke on 2/17/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

#import <Foundation/Foundation.h>

#import <UIKit/UIKit.h>
#import <opencv2/opencv.hpp>

@interface OpenCVHelper : NSObject

+ (cv::Mat)cvMatFromUIImage:(UIImage *)image;
+ (UIImage *)UIImageFromCVMat:(cv::Mat)cvMat;

@end
