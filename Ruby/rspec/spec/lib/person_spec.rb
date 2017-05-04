require 'spec_helper'
require 'person'

RSpec.describe Person do
  let(:person) { Person.new }
  describe '#teen?' do
    context '13 year old' do
      before { person.age = 13 }
      it 'teen' do
        expect(person).to be_teen
      end      
    end
    context '20 year old' do
      before { person.age = 20 }
      it 'not teen' do
        expect(person).to_not be_teen   
      end
    end
  end
end
