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
package org.apache.myfaces.extensions.validator.core.metadata.transformer;

import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
//long term target: skipValidation for all annotations (if possible)
public abstract class AbstractMetaDataTransformer implements MetaDataTransformer
{
    protected final Log logger = LogFactory.getLog(getClass());

    protected AbstractMetaDataTransformer()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public final Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        if(skipValidation(metaDataEntry))
        {
            if(logger.isTraceEnabled())
            {
                logger.trace(getClass() + " validation skiped");
            }
            Map<String, Object> results = new HashMap<String, Object>();
            results.put(CommonMetaDataKeys.SKIP_VALIDATION, getMetaDataKeys());
            return results;
        }

        return convert(metaDataEntry);
    }

    protected boolean skipValidation(MetaDataEntry metaDataEntry)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();

        Boolean result = (Boolean) ExtValUtils.getELHelper()
            .getValueOfExpression(facesContext, new ValueBindingExpression(getSkipExpression(metaDataEntry)));

        if(logger.isTraceEnabled())
        {
            logger.trace(getClass() + "#skipValidation result of getSkipExpression: "
                + getSkipExpression(metaDataEntry));
        }

        return result;
    }

    protected String getSkipExpression(MetaDataEntry metaDataEntry)
    {
        return "#{false}"; //default - don't skip
    }

    /**
     * if the current annotation is skiped provide the meta-data keys which are irrelevant
     * -> component initializers have to reset these properties
     */
    protected List<String> getMetaDataKeys()
    {
        return new ArrayList<String>();
    }

    protected abstract Map<String, Object> convert(MetaDataEntry metaData);
}
