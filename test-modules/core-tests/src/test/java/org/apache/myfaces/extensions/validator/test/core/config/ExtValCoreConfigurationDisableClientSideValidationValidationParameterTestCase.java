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
package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameter;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase(String name)
    {
        super(name);
    }

    public static interface CustomDisableClientSideValidation extends DisableClientSideValidation
    {

    }

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        if (needCustomConfig())
        {

            return new DefaultExtValCoreConfiguration()
            {

                @Override
                public Class<? extends ValidationParameter> disableClientSideValidationValidationParameter()
                {
                    return CustomDisableClientSideValidation.class;
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testDisableClientSideValidationValidationParameterDefault()
    {

        assertEquals(DisableClientSideValidation.class.getName(), ((Class) ExtValContext.getContext()
                .getGlobalProperty(DisableClientSideValidation.class.getName())).getName());
    }

    public void testDisableClientSideValidationValidationParameterCustomConfig()
    {

        assertEquals(CustomDisableClientSideValidation.class.getName(), ((Class) ExtValContext.getContext()
                .getGlobalProperty(DisableClientSideValidation.class.getName())).getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase.class);
    }

}
