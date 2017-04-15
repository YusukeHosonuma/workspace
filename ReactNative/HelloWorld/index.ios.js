import React, { Component } from 'react';
import { AppRegistry, StyleSheet, Text, Image, View } from 'react-native';

// export default class HelloWorld extends Component {
//   render() {
//     return (
//       <Text>Hello world!</Text>
//     );
//   }
// }

class Blink extends Component {
  constructor(props) {
    super(props);

    // Reduxを使ったほうが良いとのこと？
    this.state = { showText: true };
    setInterval(() => {
      this.setState({ showText: !this.state.showText })
    }, 1000);
  }

  render() {
    let display = this.state.showText ? this.props.text : ' ';
    return (
      <Text>{display}</Text>
    );
  }
}

class LotsOfStyles extends Component {
  render() {
    return (
      <View>
        <Text style={styles.red}>just red</Text>
        <Text style={styles.bigblue}>just bigblue</Text>
        <Text style={[styles.bigblue, styles.red]}>bigblue, then red</Text>
        <Text style={[styles.red, styles.bigblue]}>red, then bigblue</Text>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  bigblue: {
    color: 'blue',
    fontWeight: 'bold',
    fontSize: 30,
  },
  red: {
    color: 'red',
  },
})

export default class Images extends Component {
  render() {
    return (
      <View>
        <Blink text='Apple' />
        <Blink text='Orange' />
        <Blink text='Banana' />
      </View>
    );
  }
}

AppRegistry.registerComponent('HelloWorld', () => LotsOfStyles);
