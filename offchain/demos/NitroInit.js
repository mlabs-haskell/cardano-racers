const createActor = (name, address, balance) => `
  <div id="${name}-card" class="wallet-card">
    <h2>Actor: ${name}</h2>
    <p>Address: ${address}</p>
    <table>
      <thead>
        <tr>
          <th>Asset</th>
          <th>Value</th>
        </tr>
      </thead>
      <tbody id="balance-table-body">
          ${balance.map((b) => `<tr><td>${b[0]}</td><td>${b[1]}</td></tr>`)}
      </tbody>
    </table>
  </div>
`;

let selectedActor = "Admin";

exports.setupListeners = (handlers) => () => {

  document.getElementById("init").addEventListener("click", () => {
    handlers.initNitro().then((x) => download('params.json', x))
  })
  document.getElementById("modify-price").addEventListener("click", () => {
    handlers.modifyNitroState().then(console.log)
  })
  document.getElementById("mint-admin").addEventListener("click", () => {
    handlers.adminMintNitro().then(console.log)
  })
  document.getElementById("mint-bot").addEventListener("click", () => {
    handlers.botMintNitro().then(console.log)
  })
  document.getElementById("buy").addEventListener("click", () => {
    handlers.userBuyNitro().then(console.log)
  })

  document.getElementById("refresh-state").addEventListener("click", () => {
    document.getElementById("wallets").innerHTML = "";
    handlers.refreshState().then(statejson => {
      document.getElementById("state").textContent = statejson
    });
  })

  document.getElementById("refresh-wallets").addEventListener("click", () => {
    document.getElementById("wallets").innerHTML = "";
    handlers.refreshWallet().then(actors => {
      actors.forEach((a) => {
        document.getElementById("wallets").innerHTML += createActor(
          a.name,
          a.address,
          a.balance
        );
      });
    });
  })
};


function download(filename, text) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}

exports.promptFor = (msg) => () => {
  const x = prompt(msg)
  return x
}
