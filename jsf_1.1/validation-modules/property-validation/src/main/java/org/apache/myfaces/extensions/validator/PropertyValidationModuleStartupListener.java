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
package org.apache.myfaces.extensions.validator;

import org.apache.myfaces.extensions.validator.baseval.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.startup.AbstractStartupListener;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

/**
 * @author Gerhard Petracek
 */
public class PropertyValidationModuleStartupListener extends AbstractStartupListener
{
    protected void init()
    {
        initStaticStrategyMappings();
        initDefaultComponentInitializerName();
    }

    private void initStaticStrategyMappings()
    {
        String jpaBasedValidation = WebXmlParameter.DEACTIVATE_JPA_BASED_VALIDATION;
        if (jpaBasedValidation == null
                || !jpaBasedValidation.equalsIgnoreCase("true"))
        {
            ExtValUtils
                    .getInformationProviderBean()
                    .addStaticStrategyMappingSource(
                            ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME
                                    + ".jpa_strategy_mappings");
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "web.xml parameter to deactivate it")
    private void initDefaultComponentInitializerName()
    {
        ExtValContext.getContext().addComponentInitializer(new HtmlCoreComponentsComponentInitializer());
    }
}
