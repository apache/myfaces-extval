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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.validation.ValidatorFactory;
import java.util.logging.Logger;

/**
 * ExtVal Core Module configuration.
 * 'custom' as prefix is used for 'optional' configurations. That means
 * if a method returns null ExtVal uses a different approach to find an implementation e.g. via a naming convention
 * -> all other methods aren't allowed to return null if there is no additional rule.
 *
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class ExtValBeanValidationModuleConfiguration implements ExtValModuleConfiguration
{
    private static ExtValContext extValContext = null;

    private static final Logger LOGGER = Logger.getLogger(ExtValBeanValidationModuleConfiguration.class.getName());

    private static final String MISSING_MODULE_CONFIG_MESSAGE =
            "no config for " + ExtValBeanValidationModuleConfiguration.class.getName() + " found. " +
            "maybe the call of ExtValBeanValidationModuleConfiguration#get" +
            " is triggered before the registration process. the default config gets used.";

    protected ExtValBeanValidationModuleConfiguration()
    {
    }

    /**
     * Don't access ExtValContext during initialization of the class. E.g. OpenWebBeans initializes all classes during
     * startup of the WebContainer.
     * ({@link org.apache.myfaces.extensions.validator.core.ExtValContext}
     * constructor tries to access Web.xml parameters through
     * {@link javax.faces.context.FacesContext} which isn't available during the classpath-scanning.)
     *
     * @return The ExtValContext
     */
    private static ExtValContext getExtValContext()
    {
        if (extValContext == null)
        {
            extValContext = ExtValContext.getContext();
        }
        return extValContext;
    }

    /**
     * Returns the configuration for the core-module stored in the context.
     * If this doesn't exists (usually a startup-listener registers a (custom) implementation),
     * it returns a new instance of the default implementation.
     *
     * @return The active ExtVal Core Module Configuration
     */
    public static ExtValBeanValidationModuleConfiguration get()
    {
        ExtValBeanValidationModuleConfiguration moduleConfig =
                getExtValContext().getModuleConfiguration(ExtValBeanValidationModuleConfiguration.class);

        if(moduleConfig == null)
        {
            LOGGER.fine(MISSING_MODULE_CONFIG_MESSAGE);
        }
        return moduleConfig != null ? moduleConfig : new DefaultExtValBeanValidationModuleConfiguration();
    }

    /**
     * Sets a new configuration for the core-module
     *
     * @param config The new configuration for the core-module
     * @param forceOverride use true to replace an existing configuration
     * @return true if the new config was registered successfully
     */
    @UsageInformation(UsageCategory.INTERNAL)
    public static boolean use(ExtValBeanValidationModuleConfiguration config, boolean forceOverride)
    {
        return getExtValContext()
                .addModuleConfiguration(ExtValBeanValidationModuleConfiguration.class, config, forceOverride);
    }

    /**
     * Allows to customize the {@link ValidatorFactory}
     *
     * @return an instance of a custom {@link ValidatorFactory}, null otherwise
     */
    public abstract ValidatorFactory customValidatorFactory();

    /**
     * Allows to customize the {@link ExtValBeanValidationContext}
     * 
     * @return an instance of a custom {@link ExtValBeanValidationContext}, null otherwise
     */
    public abstract ExtValBeanValidationContext customExtValBeanValidationContext();
}
