import { EnrollStudent, Roster, LoginResponse, UserData, AuthInfo, ResetInfo, ResetPassInfo } from "@/types";

import Mock from "./api.mock";
import Server from "./api.server";

interface ApiService {
  sessionAccessToken: string | null;
  signIn(info: AuthInfo): Promise<LoginResponse>;
  isSignedIn(): boolean;
  user(token: string): Promise<UserData>;
  unauthorized(): Promise<void>;
  signOut(): Promise<void>;
  enroll(students: Roster): Promise<EnrollStudent[]>;
  roster(className: string): Promise<EnrollStudent[]>;
  reset(reset: ResetInfo): Promise<string>;
  resetPass(reset: ResetPassInfo): Promise<string>;
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
