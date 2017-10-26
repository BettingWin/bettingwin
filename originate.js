const spawn = require('child_process').spawn
const fs = require('fs')


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


const key_hash = 'tz1bV31HQMWMqJ8crvTZrv1LHiJPUQC9ZNyY'

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

      return `(Pair None (Pair "${key_hash}" (Pair "${contract}" (Pair (Left Unit) (Pair (Pair "${bet_name}" (Pair ${created_date} (Pair (Pair ${bet_time_start} ${bet_time_end} ) (Pair (Pair ${report_time_start} ${report_time_end} ) (List ${odds} ) ) ) ) ) (Pair (Pair None (Pair 0 (Map ) ) ) (Pair 0 (Pair (Map ) (Pair (Map ) (Pair (Map ) (Map ) ) ) ) ) ) ) ) ) ) )`
    }
  },
  // 'contracts/bet/bet_add_margin.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_add_margin'),
  //   contract: '',
  //   arg: () => {
  //     const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
  //     return `(Pair 0 "${contract}")`
  //   }
  // },
  // 'contracts/bet/bet_report.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_report'),
  //   contract: '',
  //   arg: () => {
  //     const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
  //     return `(Pair 0 "${contract}")`
  //   }
  // },
  // 'contracts/bet/bet_setup_odds.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_setup_odds'),
  //   contract: '',
  //   arg: () => {
  //     const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
  //     return `(Pair (Map ) "${contract}")`
  //   }
  // },
  // 'contracts/bet/bet_add_bet_mod.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_add_bet_mod'),
  //   contract: '',
  //   arg: () => {
  //     return `"${key_hash}"`
  //   }
  // },
  // 'contracts/bet/bet_add_bet_calc.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_add_bet_calc'),
  //   contract: '',
  //   arg: () => {
  //     const contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
  //     return `Unit`
  //   },
  //   pre_process: content => {
  //     const mod_contract = file_dict['contracts/bet/bet_add_bet_mod.liq.tz'].contract
  //     const contracts = [mod_contract]
  //     return content.replace(/SOURCE\s+\([^]+?;/g, (raw) => {
  //       return `PUSH (${raw.replace('SOURCE', 'contract').replace(';', '').replace(/[\r\n]+/g, '').replace(/\ +/g, ' ')}) "${contracts.shift()}";`
  //     })
  //   }
  // },
  // 'contracts/bet/bet_add_bet_check.liq.tz': {
  //   typecheck: false,
  //   alias: util.get_random_alias('bet_add_bet_check'),
  //   contract: '',
  //   arg: () => {
  //     const main_contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
  //     const calc_contract = file_dict['contracts/bet/bet_add_bet_calc.liq.tz'].contract
  //     const mod_contract = file_dict['contracts/bet/bet_add_bet_mod.liq.tz'].contract
  //     return `(Pair (Pair 0 (Pair 0 0)) "${main_contract}")`
  //   },
  //   pre_process: content => {
  //     const calc_contract = file_dict['contracts/bet/bet_add_bet_calc.liq.tz'].contract
  //     const contracts = [calc_contract]
  //     return content.replace(/SOURCE\s+\([^]+?;/g, (raw) => {
  //       return `PUSH (${raw.replace('SOURCE', 'contract').replace(';', '').replace(/[\r\n]+/g, '').replace(/\ +/g, ' ')}) "${contracts.shift()}";`
  //     })
  //   }
  // },
  'contracts/bet/bet_settle_mod.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_settle_mod'),
    contract: '',
    arg: () => {
      return `Unit`
    }
  },
  'contracts/bet/bet_settle_calc.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_settle_calc'),
    contract: '',
    arg: () => {
      return `Unit`
    },
    pre_process: content => {
      const mod_contract = file_dict['contracts/bet/bet_settle_mod.liq.tz'].contract
      const contracts = [mod_contract]
      return content.replace(/SOURCE\s+\([^]+?;/g, (raw) => {
        return `PUSH (${raw.replace('SOURCE', 'contract').replace(';', '').replace(/[\r\n]+/g, '').replace(/\ +/g, ' ')}) "${contracts.shift()}";`
      })
    }
  },
  'contracts/bet/bet_settle.liq.tz': {
    typecheck: false,
    alias: util.get_random_alias('bet_settle'),
    contract: '',
    arg: () => {
      const main_contract = file_dict['contracts/bet/bet_main.liq.tz'].contract
      return `"${main_contract}"`
    },
    pre_process: content => {
      const calc_contract = file_dict['contracts/bet/bet_settle_calc.liq.tz'].contract
      const contracts = [calc_contract]
      return content.replace(/SOURCE\s+\([^]+?;/g, (raw) => {
        return `PUSH (${raw.replace('SOURCE', 'contract').replace(';', '').replace(/[\r\n]+/g, '').replace(/\ +/g, ' ')}) "${contracts.shift()}";`
      })
    }
  }
}

const pre_process = file_name => {
  const content = fs.readFileSync(file_name)
  if (file_dict[file_name].pre_process) {
    const processed_content = file_dict[file_name].pre_process(content.toString())
    fs.writeFileSync(file_name + '.processed.tz', processed_content)
  }
}

const originate = (file_name) => {
  let file_location = file_name

  if (file_dict[file_name].pre_process) {
    pre_process(file_name)
    file_location = file_name + '.processed.tz'
  }

  return (file_dict[file_name].typecheck ? 
    alphanet(`typecheck program container:${file_location}`.split(' ')) : 
    Promise.resolve('Well typed'))
  .then(x => {
    if (x.trim() !== 'Well typed'){
      return Promise.reject(x)
    } else {
      const alias = file_dict[file_name].alias
      const arg = file_dict[file_name].arg()

      return alphanet(`originate contract ${alias} for my_identity transferring 2.01 from my_account running container:${file_location} -init`.split(' ').concat(arg))
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
