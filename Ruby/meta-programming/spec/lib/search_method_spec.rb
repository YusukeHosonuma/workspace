require 'spec_helper'
require 'search_method'

describe 'search method' do
  it 'found in superclass' do
    obj = MySubclass.new
    expect(obj.my_method).to eq 'my_method()'
  end
  it 'inheritance chain' do
    # Kernelモジュールが継承チェーンに含まれている
    expect(MySubclass.ancestors).to eq [MySubclass, MyClass, Object, Kernel, BasicObject]
  end
  xit 'Module in inheritance chain' do
    # include すると、そのクラスの上にモジュールが挿入される
    expect(D1.ancestors).to eq [D1, C1, M1, Object, Kernel, BasicObject]
  end
end
