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
package org.apache.myfaces.extensions.validator.test.propval.config;

import org.apache.myfaces.extensions.validator.PropertyValidationModuleStartupListener;
import org.apache.myfaces.extensions.validator.baseval.DefaultExtValBaseValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.baseval.ExtValBaseValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.crossval.DefaultExtValCrossValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.crossval.ExtValCrossValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.apache.myfaces.test.runners.NamedRunner;
import org.apache.myfaces.test.runners.TestPerClassLoaderRunner;
import org.junit.runner.RunWith;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
@RunWith(value = TestPerClassLoaderRunner.class)
public abstract class ExtValCrossValidationConfigurationTestCase extends AbstractExValCoreTestCase
{


    protected String getName()
    {
        return NamedRunner.getTestMethodName();
    }


    protected boolean needXmlParameters()
    {
        return getName().contains("Xml");
    }

    protected boolean needCustomConfig()
    {
        return !getName().contains("Xml") && !getName().contains("Default");
    }

    @Override
    /*
     * Made the method final because we do
     */
    protected final void invokeStartupListeners()
    {
        super.invokeStartupListeners();
        new PropertyValidationModuleStartupListener()
        {
            private static final long serialVersionUID = -3861810605160281884L;

            @Override
            protected void initModuleConfig()
            {
                ExtValBaseValidationModuleConfiguration
                        .use(new DefaultExtValBaseValidationModuleConfiguration(), false);
                ExtValCrossValidationModuleConfiguration.use(new DefaultExtValCrossValidationModuleConfiguration(),
                        false);
            }

            @Override
            protected void init()
            {
                initModuleConfig();
                super.init();
            }
        }.init();
    }

    @Override
    protected final ExtValModuleConfiguration[] getCustomConfigObjects()
    {
        if (needCustomConfig())
        {
            ExtValModuleConfiguration[] result = new ExtValModuleConfiguration[]
            { getCustomCrossValidationModuleConfiguration() };
            if (result.length == 1 && result[0] == null)
            {
                // test don't want to specify a custom configuration.
                return null;
            } else
            {
                return result;
            }
        } else
        {
            return null;
        }
    }

    abstract protected ExtValCrossValidationModuleConfiguration getCustomCrossValidationModuleConfiguration();

}
