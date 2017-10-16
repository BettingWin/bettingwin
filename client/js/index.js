(function(window, document){
  var util = {
    post: function(url, params, callback){
      var xhr = new XMLHttpRequest()
      xhr.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
          callback(this.responseText)
        }
      }
      xhr.open('POST', url, true)
      xhr.send(JSON.stringify(params))
    }
  }

  var domain = 'http://127.0.0.1:8080'
  var rpcCall = function(method, params, callback){
    var router = {
      'prevalidation': '/blocks/prevalidation',
      'bootstrapped': '/bootstrapped',
      'blocks': '/blocks',
      'inject_operation': '/inject_operation'
    }

    util.post(domain + router[method], params, callback)
  }

  window.BTW = {
    domain: domain,
    rpcCall: rpcCall    
  }
})(window, document)