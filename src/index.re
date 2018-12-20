open Reprocessing;

let pipeWidth = 50.;

let halfGap = 25.;

let birdHeight = 50.;

let gravity = 400.;

let birdX = 50.;

let floorY = 400.;

type stateT = {
  birdY: float,
  birdVY: float,
  pipes: list((float, float))
};

let setup = (env) => {
  Env.size(~width=600, ~height=600, env);
  {
   birdY: 50.,
   birdVY: 0.,
   pipes: [(200., 100.), (400., 100.), (600., 100.)]
  }
};

let draw = ({birdY, birdVY, pipes} as state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rectf(~pos=(birdX, state.birdY), ~width=50., ~height=50., env);
  Draw.fill(Utils.color(~r=41, ~g=244, ~b=150, ~a=255), env);
  Draw.rectf(
   ~pos=(0., floorY),
   ~width=float_of_int(Env.width(env)),
   ~height=float_of_int(Env.height(env)) -. floorY,
   env
  );
  List.iter(((x,y)) => {
    Draw.rectf(
     ~pos=(x, 0.),
     ~width=pipeWidth,
     ~height= y-. halfGap,
     env
    );
  }, pipes);
  {
    ...state,
    birdY: min(birdY +. birdVY *. Env.deltaTime(env), floorY -. birdHeight),
    birdVY: if(Env.keyPressed(Space, env)) {
      -200.
    } else {
      birdVY +. gravity *. Env.deltaTime(env)
    }
  }
};

run(~setup, ~draw, ());
