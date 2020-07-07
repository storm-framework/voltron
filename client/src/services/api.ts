import { LoginResponse, UserData, AuthInfo } from "@/types";

import Mock from "./api.mock";
import Server from "./api.server";

interface ApiService {
  sessionUserId: string | null;
  signIn(info: AuthInfo): Promise<LoginResponse>;
  isSignedIn(): boolean;
  user(userId: string): Promise<UserData>;
  unauthorized(): Promise<void>;
}

// const module: ApiService = Server;
const module: ApiService = Mock;

// if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
// if (0 == 1) {
//   module = Mock;
// } else {
//   module = Server;
// }

export default module;