import { EnrollStudent, Roster, LoginResponse, UserData, AuthInfo } from "@/types";

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
