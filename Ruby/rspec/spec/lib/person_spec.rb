require 'spec_helper'
require 'person'

describe Person do
  let(:person) { Person.new }
  describe '#teen?' do
    subject { person.teen? }
    context 'when 13 year old' do
      it 'teen' do
        person.age = 13
        is_expected.to be true
      end      
    end
    context 'when 20 year old' do
      it 'not teen' do
        person.age = 20
        is_expected.to be false
      end
    end
  end
  describe '#greet' do
    it 'hello' do
      expect(person.greet).to eq 'hello'
    end
  end
end
