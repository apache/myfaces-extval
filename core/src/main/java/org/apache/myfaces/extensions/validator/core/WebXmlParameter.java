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

import org.apache.myfaces.extensions.validator.util.WebXmlUtils;

/**
 * centralized in order that these information arn't spread over the complete code base
 *
 * @author Gerhard Petracek
 */
public interface WebXmlParameter {
    /*
     * custom
     */
    static final String CUSTOM_MESSAGE_BUNDLE = WebXmlUtils.getInitParameter("CUSTOM_MESSAGE_BUNDLE");
    static final String CUSTOM_EXTENSION_BASE_PACKAGE = WebXmlUtils.getInitParameter("CUSTOM_BASE_PACKAGE");
    static final String CUSTOM_CONVENTION_INFO_PROVIDER_BEAN = WebXmlUtils.getInitParameter("CUSTOM_INFORMATION_PROVIDER_BEAN");

    static final String CUSTOM_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER = WebXmlUtils.getInitParameter("CUSTOM_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER");
    static final String CUSTOM_ANNOTATION_TO_VALIDATION_STRATEGY_NAME_MAPPER = WebXmlUtils.getInitParameter("CUSTOM_ANNOTATION_TO_VALIDATION_STRATEGY_NAME_MAPPER");

    static final String CUSTOM_MESSAGE_RESOLVER_FACTORY = WebXmlUtils.getInitParameter("CUSTOM_MESSAGE_RESOLVER_FACTORY");
    static final String CUSTOM_VALIDATION_STRATEGY_FACTORY = WebXmlUtils.getInitParameter("CUSTOM_VALIDATION_STRATEGY_FACTORY");
    static final String CUSTOM_ANNOTATION_EXTRACTOR_FACTORY = WebXmlUtils.getInitParameter("CUSTOM_ANNOTATION_EXTRACTOR_FACTORY");

    static final String CUSTOM_ANNOTATION_EXTRACTOR = WebXmlUtils.getInitParameter("CUSTOM_ANNOTATION_EXTRACTOR");

    //TODO documentation
    static final String CUSTOM_VALIDATIONSTRATEGY_MAPPING = WebXmlUtils.getInitParameter("CUSTOM_STATIC_VALIDATIONSTRATEGY_MAPPING");

    /*
     * deactivate
     */
    static final String DEACTIVATE_RESTORE_PROXY_PHASE_LISTENER = WebXmlUtils.getInitParameter("DEACTIVATE_RESTORE_PROXY_PHASE_LISTENER");
    static final String DEACTIVATE_DEFAULT_CONVENTION = WebXmlUtils.getInitParameter("DEACTIVATE_DEFAULT_CONVENTION");
    static final String DEACTIVATE_PROXY_MAPPING = WebXmlUtils.getInitParameter("DEACTIVATE_PROXY_MAPPING");

    /*
     * fallback lib - if the usage of cglib is a problem
     */
    @Deprecated
    static final String CUSTOM_CONVERTER_TO_ADAPTER_NAME_MAPPER = WebXmlUtils.getInitParameter("CUSTOM_CONVERTER_TO_ADAPTER_NAME_MAPPER");
    @Deprecated
    static final String CUSTOM_CONVERTER_ADAPTER_FACTORY = WebXmlUtils.getInitParameter("CUSTOM_CONVERTER_ADAPTER_FACTORY");
    @Deprecated
    static final String USE_ADAPTERS = WebXmlUtils.getInitParameter("USE_ADAPTERS");
}