const loadModules = async () => {
  const admin = await import('./output/Lib.CardanoRacers.AdminFFI/index.js');
  const bot = await import('./output/Lib.CardanoRacers.BotFFI/index.js');
  const client = await import('./output/Lib.CardanoRacers.ClientFFI/index.js');

  return {
    admin,
    bot,
    client
  };
};

module.exports = loadModules();
