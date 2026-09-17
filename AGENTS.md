# Trading Game
 In this⠠game, there will⡀be N⡀players, each of which have a
  private number p_i. Each player can place a buy or a sell on the sum S = sum_i p_i. A
  player can see their own private number, but not any other player's private number. Players
  can only submit⠄buy orders or sell orders for⠄S. The game lasts 1 hour. Players can⠂see at
  any moment the current state of⠁the exchange. At⡀the end of the game, the value of the sum
⠐⢀resolves to S.

# Stack
- Test with quickcheck and quickspec
- Effects with `Eff`
- Use Text.Blaze.HTML for html rendering.
- use tailwindcss for styling
- use HTMX for client-side interactivity (HTMX supports SSE https://htmx.org/extensions/sse/)
- use Wai.Warp for web server.
