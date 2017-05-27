require 'spec_helper'
require 'cons'

describe Cons do
  it 'initialize' do
    xs = Cons.new(1, nil)
    expect(xs.value).to eq 1
  end
  it 'cons' do
    xs = Cons.new(1, nil)
    expect(xs.size).to eq 1

    xs = xs.cons 2
    expect(xs.size).to eq 2

    xs = xs.cons 3
    expect(xs.size).to eq 3
  end
  describe 'size' do
    context '1' do
      it '1' do
        xs = Cons.new(1, nil)
        expect(xs.size).to eq 1
      end
    end
    context '2' do
      it '2' do
        xs = Cons.new(1, Cons.new(2, nil))
        expect(xs.size).to eq 2
      end
    end
  end
  describe 'each' do
    it 'size == 1' do
      xs = Cons.new(1, nil)
      ys = []
      xs.each do |x|
        ys.push(x)
      end
      expect(ys).to eq [1]
    end
    it 'size == 3' do
      xs = Cons.new(1, Cons.new(2, Cons.new(3, nil)))
      ys = []
      xs.each do |x|
        ys.push(x)
      end
      expect(ys).to eq [1, 2, 3]
    end
  end
  
  describe 'to_array' do
    it 'size == 1' do
      xs = Cons.new(1, nil)
      expect(xs.to_array).to eq [1]
    end
    it 'size == 3' do
      xs = Cons.new(1, Cons.new(2, Cons.new(3, nil)))
      expect(xs.to_array).to eq [1, 2, 3]
    end
  end

  describe 'take' do
    let(:xs) { Cons.new(1, Cons.new(2, Cons.new(3, nil))) }
    it '1' do
      expect(xs.take(1).to_array).to eq [1]
    end
    it '3' do
      expect(xs.take(3).to_array).to eq [1, 2, 3]
    end
    it '4' do
      expect(xs.take(4).to_array).to eq [1, 2, 3]
    end
  end
end
