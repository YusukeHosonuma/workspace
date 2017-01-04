//
//  OCMockSampleTests.m
//  OCMockSampleTests
//
//  Created by Yusuke on 6/23/15.
//  Copyright (c) 2015 Yusuke. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <OCMock/OCMock.h>
#import "Foo.h"

@interface OCMockSampleTests : XCTestCase

@property (nonatomic, assign) BOOL handleNotificationMockCalled;
@property (nonatomic, assign) BOOL andDoBlockCalled;

@end

@implementation OCMockSampleTests

- (void)setUp {
    [super setUp];
    self.handleNotificationMockCalled = NO;
    self.andDoBlockCalled = NO;
}

- (void)tearDown {
    [super tearDown];
}

/// --- create

- (void)test_makeForClass {
    
    // クラス定義からMockを生成する
    id mock = [OCMockObject mockForClass:[NSString class]];
    NSInteger returnValue = 100;
    [[[mock stub] andReturnValue:[NSValue valueWithBytes:&returnValue objCType:@encode(NSInteger)]] length];
    XCTAssertEqual([mock length], 100);
}

- (void)test_makeForClassWithMacro {
    
    // マクロ版
    id mock = OCMClassMock([NSString class]);
    OCMStub([mock length]).andReturn(100); // マクロを使うと参照型かプリミティブ型か区別しなくても良さそう
    XCTAssertEqual([mock length], 100);
}

- (void)test_partialMockObject {
    
    // 生成ずみのオブジェクトからMockを生成する
    Foo *foo = [[Foo alloc] init];
    id mock = [OCMockObject partialMockForObject:foo];
    [[[mock stub] andReturn:@"foo!"] bar];
    XCTAssertEqualObjects(@"foo!", [mock bar]);
}

- (void)test_partialMockObjectWithMacro {
    
    // マクロ版
    Foo *foo = [[Foo alloc] init];
    id mock = OCMPartialMock(foo);
    OCMStub([mock bar]).andReturn(@"foo!");
    XCTAssertEqualObjects(@"foo!", [mock bar]);
}

// --- andXxx

- (void)test_andReturn {
    
    // andReturn - 任意の値（id型）を返す
    id mock = [OCMockObject mockForClass:[NSString class]];
    [[[mock stub] andReturn:@"foo!"] stringByAppendingString:[OCMArg any]];      // 対象のメソッドが引数を取るときは[OCMArg any]を与える
    [[[mock stub] andReturn:@"bar!"] stringByAppendingPathExtension:OCMOCK_ANY]; // マクロで書くならOCMOCK_ANY
    XCTAssertEqualObjects(@"foo!", [mock stringByAppendingString:@"dummy"]);
    XCTAssertEqualObjects(@"bar!", [mock stringByAppendingPathExtension:@"dummy"]);
}

- (void)test_andReturnValue {

    // andReturnValue - andReturnのプリミティブ値バージョン
    id mock = OCMClassMock([NSString class]);
    NSInteger returnValue = 100;
    [[[mock stub] andReturnValue:[NSValue valueWithBytes:&returnValue objCType:@encode(NSInteger)]] length];
    XCTAssertEqual(100, [mock length]);
}

- (void)test_andThrow {
    
    // andThrow - 例外をスローするようにする
    id mock = OCMClassMock([NSString class]);
    id exception = [NSException exceptionWithName:@"dummy" reason:@"" userInfo:nil];
    [[[mock stub] andThrow:exception] length];
    XCTAssertThrows([mock length]);
}

- (void)test_andPost {

    // andPost - Notificationをpostするようにする
    id mock = OCMClassMock([NSString class]);
    id notification = [NSNotification notificationWithName:@"mock" object:self];
    [[[mock stub] andPost:notification] length];

    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleNotificationMock:)
                                                 name:@"mock"
                                               object:nil];
    
    [mock length];
    XCTAssertTrue(self.handleNotificationMockCalled);
}

- (void)handleNotificationMock:(NSNotification *)notification {
    self.handleNotificationMockCalled = YES;
}

- (void)test_andCall {
    
    // andCall - 代わりのメソッドを呼び出すようにする
    Foo *foo = [[Foo alloc] init];
    id mockStr = OCMPartialMock(foo);
    [[[mockStr stub] andCall:@selector(mockBar) onObject:self] bar]; // SELでかわりに呼び出すメソッドを指定する
    XCTAssertEqualObjects(@"mockBar!", [mockStr bar]);
}

- (NSString *)mockBar {
    return @"mockBar!";
}

- (void)test_andDo {
    
    // andDo - 特定のBlocksを呼び出すようにする
    id mock = OCMClassMock([NSString class]);
    [[[mock stub] andDo:^(NSInvocation *invocation) {
        self.andDoBlockCalled = YES;
    }] length];
    [mock length];
    
    XCTAssertTrue(self.andDoBlockCalled);
}

- (void)test_andForwardToRealObject {
    
    // andForwardToRealObject - Mockが保持している実際のオブジェクトへ呼び出しを転送するようにする
    Foo *foo = [[Foo alloc] init];
    id mock = OCMPartialMock(foo); // PartialMockでないとNG
    [[[mock stub] andForwardToRealObject] bar];
    XCTAssertEqualObjects(@"bar!", [mock bar]);
}

// --- expect / verify

- (void)test_expect_verify {
    
    // expect - そのメソッドが呼び出されることを期待し、
    // verify - で検証する
    Foo *foo = [[Foo alloc] init];
    id mock = OCMPartialMock(foo);
    [[mock expect] hoge]; // hogeメソッドが呼び出されていること（を期待）
    [mock foo];
    [mock verify]; // 検証
}

- (void)test_expect_verifyWithDelay {
    
    // verifyWithDelay - n秒後に検証する
    Foo *foo = [[Foo alloc] init];
    id mock = OCMPartialMock(foo);
    [[mock expect] hoge];
    [mock fooDelay];
    [mock verifyWithDelay:0.2]; // 0.2秒後に検証
}

@end
