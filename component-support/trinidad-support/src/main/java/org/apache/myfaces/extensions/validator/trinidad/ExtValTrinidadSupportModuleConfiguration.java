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
package org.apache.myfaces.extensions.validator.trinidad;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class ExtValTrinidadSupportModuleConfiguration implements ExtValModuleConfiguration
{
    private static ExtValContext extValContext = null;

    protected ExtValTrinidadSupportModuleConfiguration()
    {
    }

    /**
     * Don't access ExtValContext during initialization of the class.  OpenWebBeans initializes all classes during
     * startup of the WebContainer.  extValContext constructor tries to access Web.xml parameters through FacesContext
     * which isn't available yet.
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
    
    public static ExtValTrinidadSupportModuleConfiguration get()
    {
        return getExtValContext().getModuleConfiguration(ExtValTrinidadSupportModuleConfiguration.class);
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static boolean use(ExtValTrinidadSupportModuleConfiguration config, boolean forceOverride)
    {
        return getExtValContext().addModuleConfiguration(ExtValTrinidadSupportModuleConfiguration.class, config,
                forceOverride);
    }

    /*
     * web.xml config
     */

    public abstract boolean deactivateClientSideValidation();

    public abstract boolean deactivateCoreOutputLabelInitialization();

    public abstract boolean deactivateValidationExceptionInterceptor();
}
