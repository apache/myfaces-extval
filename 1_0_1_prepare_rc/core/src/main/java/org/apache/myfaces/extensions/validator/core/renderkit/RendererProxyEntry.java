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
package org.apache.myfaces.extensions.validator.core.renderkit;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class RendererProxyEntry
{
    private boolean decodeCalled = false;
    private boolean encodeBeginCalled = false;
    private boolean encodeChildrenCalled = false;
    private boolean encodeEndCalled = false;

    private Object convertedValue = null;

    public boolean isDecodeCalled()
    {
        return decodeCalled;
    }

    public void setDecodeCalled(boolean decodeCalled)
    {
        this.decodeCalled = decodeCalled;
    }

    public boolean isEncodeBeginCalled()
    {
        return encodeBeginCalled;
    }

    public void setEncodeBeginCalled(boolean encodeBeginCalled)
    {
        this.encodeBeginCalled = encodeBeginCalled;
    }

    public boolean isEncodeChildrenCalled()
    {
        return encodeChildrenCalled;
    }

    public void setEncodeChildrenCalled(boolean encodeChildrenCalled)
    {
        this.encodeChildrenCalled = encodeChildrenCalled;
    }

    public boolean isEncodeEndCalled()
    {
        return encodeEndCalled;
    }

    public void setEncodeEndCalled(boolean encodeEndCalled)
    {
        this.encodeEndCalled = encodeEndCalled;
    }

    public Object getConvertedValue()
    {
        return convertedValue;
    }

    public void setConvertedValue(Object convertedValue)
    {
        this.convertedValue = convertedValue;
    }
}
