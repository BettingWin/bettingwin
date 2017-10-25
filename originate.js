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
        reject(`ERROR:${x}\nDATA:${data.trim()}`)
      else {
        resolve(data.trim())
      }
    })
  })
}

const util = {
  get_random_alias: (prefix) => (prefix || '') + '+' + new Buffer('' + +new Date()).toString('base64')
}

const file_lst = [
  'contracts/token.liq.tz',
  'contracts/bet/bet_main.liq.tz',
]

const file_dict = {
  'contracts/token.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('token'),
    contract: '',
    arg: () => `(Pair (Map ) (Pair (Map ) (Pair 100000 (Pair 2 (Pair "BettingWin" "BTW" ) ) ) ) )`
  },
  'contracts/bet/bet_main.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_main'),
    contract: '',
    arg: () => {
      const contract = file_dict['contracts/token.liq.tz'].contract
      const bet_name = 'Initial Game'
      const created_date = 0
      const bet_time_start = parseInt(+new Date / 1000)
      const bet_time_end = bet_time_start + 3600
      const report_time_start = bet_time_end + 1
      const report_time_end = report_time_start + 3600
      const odds = ['Option A', 'Option B', 'Option C', 'Option D'].map(x => '"' + x + '"').join(' ')

      return `(Pair None (Pair "tz1bV31HQMWMqJ8crvTZrv1LHiJPUQC9ZNyY" (Pair "${contract}" (Pair (Left Unit) (Pair (Pair "${bet_name}" (Pair ${created_date} (Pair (Pair ${bet_time_start} ${bet_time_end} ) (Pair (Pair ${report_time_start} ${report_time_end} ) (List ${odds} ) ) ) ) ) (Pair (Pair None (Pair 0 (Map ) ) ) (Pair 0 (Pair (Map ) (Pair (Map ) (Pair (Map ) (Map ) ) ) ) ) ) ) ) ) ) )`
    }
  },
  'contracts/bet/bet_add_margin.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_add_margin'),
    contract: '',
    arg: () => {
      const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
      return `(Pair 0 "${contract}")`
    }
  },
  'contracts/bet/bet_report.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_report'),
    contract: '',
    arg: () => {
      const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
      return `(Pair 0 "${contract}")`
    }
  }
}

const originate = (file_name) => {
  return (file_dict[file_name].typecheck ? 
    alphanet(`typecheck program container:${file_name}`.split(' ')) : 
    Promise.resolve('Well typed'))
  .then(x => {
    if (x.trim() !== 'Well typed'){
      return Promise.reject(x)
    } else {
      const alias = file_dict[file_name].alias
      const arg = file_dict[file_name].arg()

      return alphanet(`originate contract ${alias} for my_identity transferring 2.01 from my_account running container:${file_name} -init`.split(' ').concat(arg))
    }
  })
  .then(x => {
    const contract_id = x.match(/New contract (\w+) originated from a smart contract/)
    if (contract_id.length > 1) {
      file_dict[file_name].contract = contract_id[1]
    } else {
      return Promise.reject(x)
    }
  })
}

let promise = null
for (const file_name in file_dict){
  if (promise) {
    promise = promise.then(() => originate(file_name))
  } else {
    promise = originate(file_name)
  }
}

promise.then(() => console.log(file_dict)).catch(x => console.error(x))
