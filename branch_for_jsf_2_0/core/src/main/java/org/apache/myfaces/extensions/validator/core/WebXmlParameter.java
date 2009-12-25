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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.WebXmlUtils;

/**
 * centralized in order that these information aren't spread over the complete code base
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public interface WebXmlParameter
{
    /*
     * misc
     */
    static final String CUSTOM_MESSAGE_BUNDLE = WebXmlUtils
        .getInitParameter("CUSTOM_MESSAGE_BUNDLE");

    static final String CUSTOM_BASE_PACKAGE = WebXmlUtils
        .getInitParameter("CUSTOM_BASE_PACKAGE");

    static final String CUSTOM_INFORMATION_PROVIDER_BEAN = WebXmlUtils
        .getInitParameter("CUSTOM_INFORMATION_PROVIDER_BEAN");

    static final String CUSTOM_COMPONENT_META_DATA_EXTRACTOR = WebXmlUtils
        .getInitParameter("CUSTOM_COMPONENT_META_DATA_EXTRACTOR");

    static final String CUSTOM_VALIDATION_PARAMETER_EXTRACTOR = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_PARAMETER_EXTRACTOR");

    static final String CUSTOM_VALIDATION_PARAMETER_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_PARAMETER_FACTORY");

    static final String CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING = WebXmlUtils
        .getInitParameter("CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING");

    static final String CUSTOM_COMPONENT_INITIALIZER = WebXmlUtils
        .getInitParameter("CUSTOM_COMPONENT_INITIALIZER");

    static final String CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR");

    static final String CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR = WebXmlUtils
        .getInitParameter("CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR");

    static final String CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR = WebXmlUtils
        .getInitParameter("CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR");

    /*
     * name mapper
     */
    static final String CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER");

    static final String CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER = WebXmlUtils
        .getInitParameter("CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER");

    static final String CUSTOM_VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_NAME_MAPPER = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_NAME_MAPPER");

    /*
     * filter
     */
    static final String CUSTOM_META_DATA_STORAGE_FILTER = WebXmlUtils
        .getInitParameter("CUSTOM_META_DATA_STORAGE_FILTER");
    
    /*
     * factories
     */
    static final String CUSTOM_VALIDATION_STRATEGY_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_STRATEGY_FACTORY");

    static final String CUSTOM_MESSAGE_RESOLVER_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_MESSAGE_RESOLVER_FACTORY");

    static final String CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY");

    static final String CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY");

    static final String CUSTOM_META_DATA_TRANSFORMER_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_META_DATA_TRANSFORMER_FACTORY");

    static final String CUSTOM_STORAGE_MANAGER_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_STORAGE_MANAGER_FACTORY");

    static final String CUSTOM_FACES_MESSAGE_FACTORY = WebXmlUtils
        .getInitParameter("CUSTOM_FACES_MESSAGE_FACTORY");

    /*
     * deactivate
     */
    @Deprecated
    static final String DEACTIVATE_RENDERKIT = WebXmlUtils
        .getInitParameter("DEACTIVATE_RENDERKIT");

    //currently just used by AbstractValidationErrorMessageResolver
    static final String DEACTIVATE_DEFAULT_CONVENTION = WebXmlUtils
        .getInitParameter("DEACTIVATE_DEFAULT_CONVENTION");

    static final String DEACTIVATE_DEFAULT_NAME_MAPPERS = WebXmlUtils
        .getInitParameter("DEACTIVATE_DEFAULT_NAME_MAPPERS");
    
    static final String DEACTIVATE_EL_RESOLVER = WebXmlUtils
        .getInitParameter("DEACTIVATE_EL_RESOLVER");

    static final String DEACTIVATE_COMPONENT_INITIALIZATION = WebXmlUtils
        .getInitParameter("DEACTIVATE_COMPONENT_INITIALIZATION");

    static final String DEACTIVATE_VALIDATION_PARAMETERS = WebXmlUtils
        .getInitParameter("DEACTIVATE_VALIDATION_PARAMETERS");

    static final String DEACTIVATE_REQUIRED_INITIALIZATION = WebXmlUtils
        .getInitParameter("DEACTIVATE_REQUIRED_INITIALIZATION");

    //there is nothing like DEACTIVATE_DEFAULT_VALIDATION_INTERCEPTOR
    //use ExtValContext.getContext().denyRendererInterceptor(...) within an extval-StartupListener

    /*
     * spec parameters
     */
    static final String INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL = WebXmlUtils
        .getInitParameter("javax.faces", "INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL");

    static final String VALIDATE_EMPTY_FIELDS = WebXmlUtils
        .getInitParameter("javax.faces", "VALIDATE_EMPTY_FIELDS");
}
