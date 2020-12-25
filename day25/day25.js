const fs = require('fs').promises;

const modulo = 20201227;

function transform(subject, loopSize) {
    let result = 1;
    for (let i = 0; i < loopSize; i++) {
        result *= subject;
        result %= modulo;
    }
    return result;
}

function findLoopSize(subject, pk) {
    let result = 1;
    for (let i = 0; ; i++) {
        if (result === pk) {
            return i;
        }
        result *= subject;
        result %= modulo;
    }
}

async function main() {
    const input = await fs.readFile('resources/input.txt', { encoding: 'utf8' });
    const [doorPk, cardPk] = input.split('\n').filter(s => s).map(s => parseInt(s, 10));
    console.log(`Door PK: ${doorPk}, Card PK: ${cardPk}`);
    const subject = 7;
    const doorLoopSize = findLoopSize(subject, doorPk);
    const cardLoopSize = findLoopSize(subject, cardPk);
    console.log(`Door loop size: ${doorLoopSize}, Card loop size: ${doorLoopSize}`);
    console.log(transform(doorPk, cardLoopSize) + ' -- ' + transform(cardPk, doorLoopSize));
}

main();
