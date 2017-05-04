require 'spec_helper'
require 'open_class'

describe 'open class' do

  it 'String#to_alphanumeric' do
    expect('Hello# world*'.to_alphanumeric).to eq 'Hello world'
  end

  describe D do
    it 'call x and y' do
      d = D.new
      expect(d.x).to eq 'x'
      expect(d.y).to eq 'y'
    end
  end

  it 'instance_methods' do
    expect(String.instance_methods).to eq 'abc'.methods
    expect(String.methods).not_to eq 'abc'.methods
  end

  it "String is Class's object" do
    expect('hello'.class).to eq String
    expect(String.class).to eq Class
  end

  it 'Class instance' do
    methods = Class.instance_methods(false) # false は継承したメソッドを除外、という意味
    expect(methods).to include(:allocate)
    expect(methods).to include(:new)
    expect(methods).to include(:superclass)
  end

  it "Array's superclass" do
    expect(Array.superclass).to       eq Object
    expect(Object.superclass).to      eq BasicObject
    expect(BasicObject.superclass).to eq nil
  end

  it "Class's superclass is Module" do
    expect(Class.superclass).to eq Module
  end

  # Moduleはネームスペースとしても機能する
  it "Module" do
    expect(M::C::X).to eq 'X' # `::`で階層をたどる
  end

  it "Module#constants" do
    expect(M.constants).to include :C # クラスも単なる定数
    expect(M.constants).to include :Y
  end

  describe "Class structure" do
    it 'Object.class == Class' do
      expect(Object.class).to eq Class
    end
    it 'Module.superclass == Object' do
      expect(Module.superclass).to eq Object
    end
    it 'Class.class == Class' do
      expect(Class.class).to eq Class
    end
  end

  it 'instance_variable_set' do
    obj = MyClass.new
    obj.instance_variable_set('@x', 10) # `obj`のみに追加される
    expect(obj.instance_variables).to include :@x
    expect(MyClass.instance_variables).to_not include :@x
  end

end
