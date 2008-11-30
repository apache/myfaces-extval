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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;

/**
 * Allows to intercept validatior exceptions.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ValidationExceptionInterceptor
{
    /**
     *
     * @param uiComponent the current component
     * @param metaDataEntry the meta data entry which contains the meta data of the property
     * @param convertedObject the converted user input
     * @param validatorException the current exception
     * @return false to stop throwing the exception
     */
    boolean afterThrowing(UIComponent uiComponent,
                          MetaDataEntry metaDataEntry,
                          Object convertedObject,
                          ValidatorException validatorException,
                          ValidationStrategy validatorExceptionSource);
}
