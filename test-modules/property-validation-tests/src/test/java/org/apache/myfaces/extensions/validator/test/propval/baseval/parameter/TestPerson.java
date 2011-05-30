/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.test.propval.baseval.parameter;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.junit.Ignore;

@Ignore
public class TestPerson
{
    @Required(parameters = {ViolationSeverity.Warn.class, TestAllowClientSideValidation.class})
    private String firstName;

    @Required(parameters = {
            ViolationSeverity.Info.class,
            TestDenyClientSideValidation.class,
            TestPriorityHigh.class,
            TestValidationInterceptor.class,
            DisableClientSideValidation.class,
            //LoginValidator.class,
            AdditionalValidator.class})
    private String lastName;

    private int failedLogins = 0;
    private boolean userLocked;

    /*
     * TODO these tests work in an ide but not via commandline - it's a Surefire issue
     */
    /*
    public class LoginValidator extends TestValidatorProvider
    {
        @ParameterValue
        public TestValidationStrategyProvider getValue()
        {
            return this;
        }

        @Override
        public ValidationStrategy getValidationStrategy()
        {
            return new ValidationStrategy() {

                int failedLogins;

                public void validate(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject)
                {
                    if((this.failedLogins = isLoginSuccessful()) > 0)
                    {
                        if(this.failedLogins > 3)
                        {
                            lock();
                        }
                    }
                }
            };
        }
    }
    */

    private int isLoginSuccessful()
    {
        //force an exception
        return ++this.failedLogins;
    }

    public boolean isLocked()
    {
        return userLocked;
    }

    private void lock()
    {
        this.userLocked = true;
    }

    /*
     * generated
     */

    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
    }

    public String getLastName()
    {
        return lastName;
    }

    public void setLastName(String lastName)
    {
        this.lastName = lastName;
    }
}