import { AppState, AuthInfo } from "@/types";

import Mock from "./api.mock";
// import Server from "./api.server";

interface ApiService {
  signIn(info: AuthInfo): Promise<AppState>;

  isSignedIn(): boolean;
}

// let module: ApiService;
// if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
//   module = Mock;
// } else {
//   module = Server;
// }

const module = Mock;

export default module;
