import { LoginResponse, AuthInfo } from "@/types";

import Mock from "./api.mock";
import Server from "./api.server";

interface ApiService {
  signIn(info: AuthInfo): Promise<LoginResponse>;
  isSignedIn(): boolean;
}

const module: ApiService = Server;
// const module: ApiService = Mock;

// if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
// if (0 == 1) {
//   module = Mock;
// } else {
//   module = Server;
// }

export default module;