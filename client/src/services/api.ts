import {
  AuthInfo,
  ClassLangInfo,
  EnrollStudent,
  ResetInfo,
  ResetPassInfo,
  Roster,
  SetGroup,
  User,
  UserData
} from "@/types";
import Server from "./api.server";


interface ApiService {
  signIn(info: AuthInfo): Promise<User>;
  userMe(): Promise<UserData>;
  signOut(): Promise<void>;
  enroll(students: Roster): Promise<EnrollStudent[]>;
  roster(className: string): Promise<EnrollStudent[]>;
  reset(reset: ResetInfo): Promise<string>;
  resetPass(reset: ResetPassInfo): Promise<string>;
  setLanguage(info: ClassLangInfo): Promise<string>;
  setGroup(info: SetGroup): Promise<string>;
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
