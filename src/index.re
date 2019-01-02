open Reprocessing;

let speed = 250.;

let pipeWidth = 50.;

let halfGap = 75.;

let birdSize = 20.;

let gravity = 400.;

let defaultBirdX = 50.;
let defaultBirdY = 50.;

let floorY = 400.;

type runningT = 
  | Running 
  | Dead 
  | Restart;

type stateT = {
  birdY: float,
  birdX: float,
  birdVY: float,
  pipes: list((float, float)),
  xOffset: float,
  running: runningT
};

let setup = (env) => {
  Env.size(~width=600, ~height=600, env);
  {
   birdX: defaultBirdX,
   birdY: defaultBirdY,
   birdVY: 0.,
   pipes: [(200., 100.), (400., 100.), (600., 100.), (800., 100.)],
   xOffset: 0.,
   running: Running
  }
};

let generatePipe = (x) => {
  (
    x +. Utils.randomf(~min=150., ~max=300.), 
    Utils.randomf(~min=50. +. halfGap, ~max=floorY -. 50. -. halfGap)
  )
};

let generateNewPipes = ({pipes, xOffset}, env) =>
  List.map(
    ((x, y) as pipe) =>
      if (x -. xOffset +. pipeWidth <= 0.) {
        let newX = List.fold_left((maxX, (x, _)) => max(maxX, x), 0., pipes);
        generatePipe(newX);
      } else {
        pipe
      },
      pipes
  );

let drawBird = (state, env) => {
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.ellipsef(~center=(state.birdX, state.birdY), ~radx=birdSize, ~rady=birdSize, env);
}

let draw = ({birdX, birdY, birdVY, pipes, xOffset, running} as state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=244, ~b=150, ~a=255), env);
  Draw.rectf(
   ~pos=(0., floorY),
   ~width=float_of_int(Env.width(env)),
   ~height=float_of_int(Env.height(env)) -. floorY,
   env
  );
  List.iter(((x,y)) => {
    Draw.rectf(
     ~pos=(x -. xOffset, 0.),
     ~width=pipeWidth,
     ~height= y-. halfGap,
     env
    );
    Draw.rectf(
     ~pos=(x -. xOffset, y +. halfGap),
     ~width=pipeWidth,
     ~height=float_of_int(Env.height(env)),
     env
    );
  }, pipes);
  drawBird(state, env);
  let collided = List.exists(((x,y)) => {
    Utils.intersectRectCircle(
      ~rectPos=(x -. xOffset, 0.), 
      ~rectW=pipeWidth, 
      ~rectH=y -. halfGap, 
      ~circlePos=(birdX, birdY),
      ~circleRad=birdSize
    )
    ||
    Utils.intersectRectCircle(
      ~rectPos=(x -. xOffset, y +. halfGap), 
      ~rectW=pipeWidth, 
      ~rectH=float_of_int(Env.height(env)), 
      ~circlePos=(birdX, birdY), 
      ~circleRad=birdSize
    )
  }, pipes) 
  let pipes = generateNewPipes(state, env);
  let hitFloor = birdY >= floorY -. birdSize;
  let deltaTime = Env.deltaTime(env);
  switch (running) {
  | Running => {
      ...state,
      pipes,
      birdY: max(min(birdY +. birdVY *. deltaTime, floorY -. birdSize), birdSize),
      birdVY: Env.keyPressed(Space, env) ? -200. : birdVY +. gravity *. deltaTime,
      xOffset: xOffset +. speed *. deltaTime,
      running: collided ? Dead : Running
    }
  | Dead => {
      ...state,
      pipes,
      birdX: birdX +. speed *. deltaTime,
      birdY: max(min(birdY +. birdVY *. deltaTime, floorY -. birdSize), birdSize),
      birdVY: birdVY +. gravity *. deltaTime,
      running: hitFloor ? Restart : Dead
    }
  | Restart => Env.keyPressed(Space, env) ? 
      {
        ...state,
        pipes: [generatePipe(200.), generatePipe(400.), generatePipe(600.), generatePipe(800.)],
        birdX: defaultBirdX, 
        birdY: defaultBirdY, 
        birdVY: 0., 
        xOffset: 0., 
        running: Running
      } : state
  }
};

run(~setup, ~draw, ());
