//
//  SampleViewController.m
//  OpenCVSample
//
//  Created by Yusuke on 2/17/16.
//  Copyright © 2016 Yusuke. All rights reserved.
//

#import "SampleViewController.h"

#import <opencv2/opencv.hpp>
#import <opencv2/stitching.hpp>
#import <opencv2/highgui/highgui.hpp>

#import "OpenCVHelper.h"

typedef NS_ENUM(NSUInteger, CVFeatureDetectorType) {
    CVFeatureDetectorTypeSURF,
    CVFeatureDetectorTypeSIFT,
    CVFeatureDetectorTypeORB,
    CVFeatureDetectorTypeAKAZE,
};

@implementation SampleViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    NSLog(@"test");
    
    /*
    UIImage *sharo = [UIImage imageNamed:@"IMG_3729.PNG"];
//    UIImage *greySharo = [self greyImage:sharo];
    UIImage *pointOfSharo = [self detectKeypoints:sharo];
    self.imageView.image = pointOfSharo;
    */
    
    UIImage *image1 = [UIImage imageNamed:@"IMG_3732.PNG"];
    UIImage *image2 = [UIImage imageNamed:@"IMG_3731.PNG"];
//    UIImage *image1 = [UIImage imageNamed:@"2.png"];
//    UIImage *image2 = [UIImage imageNamed:@"1.png"];
    
    UIImage *result = [self match:image1 image2:image2];
    self.imageView.image = result;
}

- (UIImage *)greyImage:(UIImage *)srcImage {
    
    cv::Mat srcMat = [OpenCVHelper cvMatFromUIImage:srcImage];
    cv::Mat greyMat;
    cv::cvtColor(srcMat, greyMat, CV_BGR2GRAY);
    
    return [OpenCVHelper UIImageFromCVMat:greyMat];
}

- (UIImage *)match:(UIImage *)image1 image2:(UIImage *)image2 {
    
    cv::Mat scene1 = [OpenCVHelper cvMatFromUIImage:image1];
    cv::Mat scene2 = [OpenCVHelper cvMatFromUIImage:image2];
    
    cv::Mat gray1;
    cv::Mat gray2;
    
    cv::cvtColor(scene1, gray1, cv::COLOR_BGR2GRAY);
    cv::cvtColor(scene2, gray2, cv::COLOR_BGR2GRAY);
    
//    cv::FeatureDetector detector(1000);
//    cv::DescriptorExtractor extrator;
    
    auto algorithm = cv::AKAZE::create();
    
    // 特徴点抽出
    std::vector<cv::KeyPoint> keypoint1, keypoint2;
    algorithm->detect(scene1, keypoint1);
    algorithm->detect(scene2, keypoint2);
    
    // 特徴記述
    cv::Mat descriptor1, descriptor2;
    algorithm->compute(scene1, keypoint1, descriptor1);
    algorithm->compute(scene2, keypoint2, descriptor2);
    
    // マッチング (アルゴリズムにはBruteForceを使用)
    cv::Ptr<cv::DescriptorMatcher> matcher = cv::DescriptorMatcher::create("BruteForce");
    std::vector<cv::DMatch> match, match12, match21;
    matcher->match(descriptor1, descriptor2, match);
    /*
    matcher->match(descriptor1, descriptor2, match12);
    matcher->match(descriptor2, descriptor1, match21);
    //クロスチェック(1→2と2→1の両方でマッチしたものだけを残して精度を高める)
    for (size_t i = 0; i < match12.size(); i++)
    {
        cv::DMatch forward = match12[i];
        cv::DMatch backward = match21[forward.trainIdx];
        if (backward.trainIdx == forward.queryIdx)
        {
            match.push_back(forward);
        }
    }
     */
    
    // マッチング結果の描画
    cv::Mat dest;
    cv::drawMatches(scene1, keypoint1, scene2, keypoint2, match, dest);
    
    std::vector<cv::Vec2f> points1(match.size());
    std::vector<cv::Vec2f> points2(match.size());
    
    for( size_t i = 0 ; i < match.size() ; ++i )
    {
        points1[i][0] = keypoint1[match[i].queryIdx].pt.x;
        points1[i][1] = keypoint1[match[i].queryIdx].pt.y;
        
        points2[i][0] = keypoint2[match[i].trainIdx].pt.x;
        points2[i][1] = keypoint2[match[i].trainIdx].pt.y;
    }
    
    //マッチング結果の書き出し
    //cv::imwrite(scene_12_path, dest);
    
    cv::Mat homo = cv::findHomography(points1, points2, CV_RANSAC);
    
    cv::Mat result;
    cv::warpPerspective(scene1, result, homo, cv::Size((int)(scene1.cols * 1.1), (int)(scene1.rows * 1.5)));
    
    // スティッチでパノラマ画像作ってみる！
    /*
    auto stitcher = cv::Stitcher::createDefault();
    
    // cv::Stitcher::stitch() のみでスティッチングを行える。
    cv::Mat panorama;
    
    std::vector< cv::Mat > imageList;
    imageList.push_back(scene1);
    imageList.push_back(scene2);
    
    auto status = stitcher.stitch(imageList, panorama);
    if( status != cv::Stitcher::OK ) {
        NSLog(@"残念。。。");
    }
     */

    for (int y = 0; y < scene1.rows; y++){
        for (int x = 0; x < scene1.cols; x++){
            result.at<cv::Vec3b>(y, x) = scene2.at<cv::Vec3b>(y, x);
        }
    }
    
    return [OpenCVHelper UIImageFromCVMat:result];
}

- (UIImage *)detectKeypoints:(UIImage *)srcImage {
    
    cv::Mat srcMat = [OpenCVHelper cvMatFromUIImage:srcImage];
    
    auto algorithm = cv::AKAZE::create();
    
    // 特徴点抽出
    std::vector<cv::KeyPoint> keypoints;
    algorithm->detect(srcMat, keypoints);
    
    printf("%lu keypoints are detected.\n", keypoints.size());
    
    // 特徴点を描画
    cv::Mat dstMat;
    
    dstMat = srcMat.clone();
    for(int i = 0; i < keypoints.size(); i++) {
        
        cv::KeyPoint *point = &(keypoints[i]);
        cv::Point center;
        int radius;
        center.x = cvRound(point->pt.x);
        center.y = cvRound(point->pt.y);
        radius = cvRound(point->size*0.25);
        
        cv::circle(dstMat, center, radius, cv::Scalar(255,255,0));
    }
    
    return [OpenCVHelper UIImageFromCVMat:dstMat];
}
     
@end
