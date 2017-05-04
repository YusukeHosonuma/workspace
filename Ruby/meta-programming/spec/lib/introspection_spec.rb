require 'spec_helper'
require 'introspection'

describe 'introspection' do
  let(:greeting) { Greeting.new('hello') }
  it '#class' do
    expect(greeting.class.to_s).to eq 'Greeting'
  end
  it '#class.instance_methods(false)' do
    expect(greeting.class.instance_methods(false)).to eq [:welcome]
  end
  it '#instance_variables' do
    expect(greeting.instance_variables).to eq [:@text]
  end
end
