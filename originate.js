const spawn = require('child_process').spawn

const alphanet = (args) => {
  return new Promise((resolve, reject) => {
    console.log(`Running: [${args}]`)

    const p = spawn('alphanet', ['client'].concat(args), {stdio: [process.stdin, 'pipe', 'pipe']})

    let data = ''
    p.stdout.on('data', (x) => {
      data += x
    })
    p.stdout.on('err', (x) => {
      data += x
    })
    p.on('close', x => {
      if (x)
        reject(x)
      else {
        console.log(`Output for: [${args}]`)
        resolve(data.trim())
      }
    })
  })
}

const file_lst = [
  'contracts/token.liq.tz',
  'contracts/bet/bet_main.liq.tz',
]

file_lst.forEach(file => {
  alphanet(`typecheck program container:${file}`.split(' '))
  .then(x => console.log(x))
  .catch(x => console.error(x))
})
