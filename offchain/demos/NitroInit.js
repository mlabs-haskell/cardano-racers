const createActor = (name, address, balance) => `
<label class="wallet-card">
  <input id="${name}-card" type="radio" name="wallet" value="${name}">
  <div class="card-content">
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
          ${balance.map(b => `<tr><td>${b[0]}</td><td>${b[1]}</td></tr>`).join("")}
      </tbody>
    </table>
  </div>
</label>
`;

const createDepositScript = (address, balance) => `
<div class="wallet-card deposit-script">
  <div class="card-content">
    <h2>Deposit Script</h2>
    <p>Address: ${address}</p>
    <table>
      <thead>
        <tr>
          <th>Asset</th>
          <th>Value</th>
        </tr>
      </thead>
      <tbody id="balance-table-body">
          ${balance.map(b => `<tr><td>${b[0]}</td><td>${b[1]}</td></tr>`)}
      </tbody>
    </table>
  </div>
</div>
`

const wrapLoading = (p) => {
  const setLoading = (b) => {
    if (b) {
      document.getElementById("loading").style.display = "block";
    } else {
      document.getElementById("loading").style.display = "none";
    }
  }
  setLoading(true);
  return p.finally(() => setLoading(false));
}
 
exports._getSelectedActor = maybe => () => {
  const selectedWallet = document.querySelector('input[name="wallet"]:checked');
  if (selectedWallet) return maybe.just(selectedWallet.value);
  return maybe.nothing;
}

exports._getParams = () => document.getElementById("params-input").value;



exports.setupListeners = handlers => () => {
  // document.getElementById("mint-driver").addEventListener("click", () => {
  //   wrapLoading(handlers.mintDriver()).then(console.log)
  // });
  const fillForm = (assetOption) => {
    // Set the input field values to the corresponding AssetOption values
    const nameInput = document.getElementById('name');
    nameInput.value = assetOption.name;

    const assetTypeRadio = document.querySelector(`input[name="asset-type"][value="${assetOption.assetType}"]`);
    assetTypeRadio.checked = true;

    const imageUrlInput = document.getElementById('image-url');
    imageUrlInput.value = assetOption.imageUrl;

    const descriptionTextarea = document.getElementById('description');
    descriptionTextarea.value = assetOption.description;
  }

  document.getElementById("init").addEventListener("click", () => {
    wrapLoading(handlers.initRacersState()).then(x => download("params.json", x))
  });
  document.getElementById("reset").addEventListener("click", () => {
    wrapLoading(handlers.resetTokens()).then(console.log)
  });
  document.getElementById("modify-state").addEventListener("click", () => {
    wrapLoading(handlers.modifyRacersState()).then(console.log);
  });
  document.getElementById("mint-nitro").addEventListener("click", () => {
    wrapLoading(handlers.mintNitro()).then(console.log);
  });
  document.getElementById("buy").addEventListener("click", () => {
    wrapLoading(handlers.userBuyNitro()).then(console.log);
  });
  document.getElementById("request-asset").addEventListener("click", () => {
    wrapLoading(handlers.makeAssetRequest()).then(console.log);
  });
  document.getElementById("redeem-requests").addEventListener("click", () => {
    wrapLoading(handlers.redeemRequests()).then(console.log);
  });
  document.getElementById("create-race").addEventListener("click", () => {
    wrapLoading(handlers.createRace()).then(console.log);
  });
  document.getElementById("close-race-manual").addEventListener("click", () => {
    wrapLoading(handlers.closeRaceManual()).then(console.log);
  });
  document.getElementById("close-race").addEventListener("click", () => {
    wrapLoading(handlers.closeRace()).then(console.log);
  });
  document.getElementById("register").addEventListener("click", () => {
    wrapLoading(handlers.registerInRace()).then(console.log);
  });
  document.getElementById("race-with-assets").addEventListener("click", () => {
    wrapLoading(handlers.raceWithAssets()).then(console.log);
  });


  const selectedRarity = document.getElementById("rarity");

  selectedRarity.addEventListener("change", () => {
    handlers.getAvailableAssets().then(assets => {
      fillForm(assets[selectedRarity.value])
    })
  });
  
  document.getElementById("asset-form").addEventListener("submit", (e) => {
    e.preventDefault();
    const selectRarity = document.getElementById('rarity');
    const rarity = selectRarity.value;

    const nameInput = document.getElementById('name');
    const name = nameInput.value;

    const nitroAmountInput = document.getElementById('nitroAmount');
    const nitroAmount = nitroAmountInput.value;

    const assetTypeRadio = document.querySelector('input[name="asset-type"]:checked');
    const assetType = assetTypeRadio.value;

    const imageUrlInput = document.getElementById('image-url');
    const imageUrl = imageUrlInput.value;

    const descriptionTextarea = document.getElementById('description');
    const description = descriptionTextarea.value;
     
    console.log({name, assetType, imageUrl, description, nitroAmount})
    handlers.setAssetOption(rarity, {name, assetType, imageUrl, description, nitroAmount })
  })
   
  document.getElementById("refresh-requests").addEventListener("click", () => {
    document.getElementById("state").textContent = "";
    handlers.refreshRequests().then(requestsjson => {
      document.getElementById("state").textContent = JSON.stringify(
        JSON.parse(requestsjson),
        null,
        2
      );
    });
  });

  document.getElementById("refresh-state").addEventListener("click", () => {
    document.getElementById("state").textContent = "";
    handlers.refreshState().then(statejson => {
      document.getElementById("state").textContent = JSON.stringify(
        JSON.parse(statejson),
        null,
        2
      );
    });
  });

  document.getElementById("refresh-registry").addEventListener("click", () => {
    document.getElementById("state").textContent = "";
    handlers.refreshRace().then(registryjson => {
      document.getElementById("state").textContent = JSON.stringify(
        JSON.parse(registryjson),
        null,
        2
      );
    });
  })

  const refreshWallets = () => {
    document.getElementById("wallets").innerHTML = "";
    handlers.refreshWallet().then(({wallets, depositScript}) => {
      if (depositScript.length) {
        const depHtml = createDepositScript(depositScript[0].address, depositScript[0].balance)
        document.getElementById("deposit-script-container").innerHTML = depHtml;
      }
      wallets.forEach(a => {
        document.getElementById("wallets").innerHTML += createActor(
          a.name,
          a.address,
          a.balance
        );
      });
    });
  };

  document.getElementById("refresh-wallets").addEventListener("click", () => {
    refreshWallets();
  });

  // refreshWallets();
};

function download(filename, text) {
  var element = document.createElement("a");
  element.setAttribute(
    "href",
    "data:text/plain;charset=utf-8," + encodeURIComponent(text)
  );
  element.setAttribute("download", filename);

  element.style.display = "none";
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}

exports.promptFor = msg => () => {
  const x = prompt(msg);
  return x;
};
