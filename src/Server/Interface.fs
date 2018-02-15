module Interface

type LoginRequest =
    { name : string
      password : string }

type LoginSuccessful =
    { name : string }

type LoginResponse =
    | LoginSuccess of LoginSuccessful
    | LoginFailure
